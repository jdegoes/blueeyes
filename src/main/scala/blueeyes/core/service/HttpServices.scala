package blueeyes.core.service

import scalaz.{Success, Validation, Failure}
import scalaz.Scalaz._
import blueeyes.json.JsonAST.JValue
import blueeyes.core.http._
import annotation.tailrec
import java.net.URLDecoder._
import blueeyes.core.http.HttpHeaders.{`Content-Type`, `Accept-Encoding`}
import blueeyes.concurrent.Future
import blueeyes.core.data.{ByteChunk, Bijection, AggregatedByteChunk, ZLIBByteChunk, GZIPByteChunk, CompressedByteChunk}
import blueeyes.util.metrics.DataSize
import blueeyes.json.JsonAST.{JField, JObject}
import xml.NodeSeq

object HttpServices{
  sealed trait NotServed {
    def or[A](result: => Validation[NotServed, A]): Validation[NotServed, A]
  }

  case class DispatchError(exception: HttpException) extends NotServed {
    override def or[A](result: => Validation[NotServed, A]) = this.fail[A]
  }

  case object Inapplicable extends NotServed {
    override def or[A](result: => Validation[NotServed, A]) = result
  }

  sealed trait Metadata
  case object FailureMetadata                                 extends Metadata

  case class DataSizeMetadata   (dataSize: Option[DataSize])  extends Metadata
  case class ContentMetadata    (mimeType: MimeType)          extends Metadata
  case class PathPatternMetadata(pattern: RestPathPattern)    extends Metadata
  case class HttpMethodMetadata (method: HttpMethod)          extends Metadata
  case class DescriptionMetadata(description: String)         extends Metadata

  sealed trait HttpService[A, B] { self =>
    def service: HttpRequest[A] => Validation[NotServed, B]

    def map[C](f: B => C): HttpService[A, C] = new HttpService[A, C] {
      override val service = (r: HttpRequest[A]) => self.service(r).map(f)
      override val metadata = self.metadata
    }

    def contramap[C](f: C => A): HttpService[C, B] = new HttpService[C, B] {
      override val service = (r: HttpRequest[C]) => self.service(r.copy(content = r.content.map(f)))
      override val metadata = self.metadata
    }

    def metadata: Option[Metadata]
  }

  case class OrService[A, B](services: HttpService[A, B]*) extends HttpService[A, B] {
    /*@tailrec*/ private def pick(r: HttpRequest[A], services: Seq[HttpService[A, B]]): Validation[NotServed, B] = services.headOption match {
      case None => Inapplicable.fail
      case Some(service) => service.service(r) match {
        case success: scalaz.Success[_, _] => success
        case Failure(notServed) => notServed or pick(r, services.tail)
      }
    }
    override val service = (r: HttpRequest[A]) => pick(r, services)

    lazy val metadata = None
  }

  trait DelegatingService[A, B, C, D] extends HttpService[A, B] {
    val delegate: HttpService[C, D]
  }

  case class PathService[A, B](path: RestPathPattern, delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
    val service = {r: HttpRequest[A] =>
      if (path.isDefinedAt(r.subpath)) {
        val pathParameters = path(r.subpath).map(parameter => (parameter._1, decode(parameter._2, "UTF-8")))

        val shiftedRequest = path.shift(r)

        delegate.service(shiftedRequest.copy(parameters = shiftedRequest.parameters ++ pathParameters))
      } else {
        Inapplicable.fail[B]
      }
    }
    lazy val metadata = Some(PathPatternMetadata(path))
  }

  case class HttpMethodService[A, B](method: HttpMethod, delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
    val service = (r: HttpRequest[A]) => if (r.method == method) delegate.service(r) else Inapplicable.fail[B]

    lazy val metadata = Some(HttpMethodMetadata(method))
  }

  case class HttpHandlerService[A, B](h: HttpRequestHandlerFull3[A, B], metadata: Option[Metadata]) extends HttpService[A, B]{
    val service = (r: HttpRequest[A]) => Success(h(r))
  }

  case class FailureService[A, B](onFailure: HttpRequest[A] => (HttpFailure, String)) extends HttpService[A, B]{
    val service = (r: HttpRequest[A]) => Failure(DispatchError(onFailure(r).fold(HttpException(_, _))))

    val metadata = None
  }

  case class CommitService[A, B](onFailure: HttpRequest[A] => (HttpFailure, String), delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
    val service = (r: HttpRequest[A]) => delegate.service(r) match {
      case Failure(Inapplicable) => Failure(DispatchError(onFailure(r).fold(HttpException(_, _))))
      case success => success
    }

    val metadata = None
  }

  case class JustTryService[A, B](delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
    def service = (r: HttpRequest[A]) => delegate.service(r)

    val metadata = None
  }

  case class AcceptService[T, S, U](mimeType: MimeType, delegate: HttpService[T, S])(implicit b: Bijection[U, T]) extends DelegatingService[U, S, T, S] {
    def service = {r: HttpRequest[U] =>
      def newContent(r: HttpRequest[U]) = try {
        Success(r.content.map(b.apply _))
      }
      catch {
        case ex => Inapplicable.fail
      }

      def handle(r: HttpRequest[U]) = newContent(r).flatMap(content => delegate.service(r.copy(content = content)))

      r.mimeTypes.find(_ == mimeType).map { mimeType => handle(r) }.orElse { r.content.map(v => handle(r)) }.getOrElse(Inapplicable.fail)
    }

    lazy val metadata = Some(ContentMetadata(mimeType))
  }

  case class GetRangeService[T, S](h: (List[(Long, Long)], String) => HttpRequestHandlerFull3[T, S], metadata: Option[Metadata]) extends HttpService[T, S]{
    def service = {r: HttpRequest[T] => extractRange(r.headers.raw) match{
      case Success(range)   => Success(h.tupled(range)(r))
      case Failure(failure) => Failure(failure)
    }}

    private def extractRange(headers: Map[String, String]): Validation[NotServed, (List[(Long, Long)], String)]  = {
      headers.collect {
        case (name, value) if name.toLowerCase == "range" =>
          value.split("=").toList match {
            case unit :: specifiers :: Nil =>
              val ranges = specifiers.split(",").toList.map {
                _.split("-").toList.map(_.trim.toLong) match {
                  case lowerBound :: upperBound :: Nil => Some((lowerBound, upperBound))
                  case _ => None
                }
              }
              ranges.find(_ == None).map(x => Inapplicable.fail).getOrElse{
                val pureRanges = ranges.collect{case Some(x) => x}
                Success[NotServed, (List[(Long, Long)], String)]((pureRanges, unit.trim.toLowerCase))
              }
            case _ => Inapplicable.fail
          }
      }.headOption.getOrElse(Inapplicable.fail)
    }
  }

  case class ProduceService[T, S, V](mimeType: MimeType, delegate: HttpService[T, Future[HttpResponse[S]]])(implicit b: Bijection[S, V]) extends DelegatingService[T, Future[HttpResponse[V]], T, Future[HttpResponse[S]]]{
    def service = {r: HttpRequest[T] =>
      delegate.service(r).map { f: Future[HttpResponse[S]] =>
        f.map{response => response.copy(content = response.content.map(b.apply), headers = response.headers + `Content-Type`(mimeType))}
      }
    }

    lazy val metadata = Some(ContentMetadata(mimeType))
  }

  case class CompressService(delegate: HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]]) extends DelegatingService[ByteChunk, Future[HttpResponse[ByteChunk]], ByteChunk, Future[HttpResponse[ByteChunk]]]{
    def service = {r: HttpRequest[ByteChunk] =>
      delegate.service(r).map{future =>
        future.map{response =>
          val encodings  = r.headers.header(`Accept-Encoding`).map(_.encodings.toList).getOrElse(Nil)
          val supported  = CompressService.supportedCompressions.filterKeys(encodings.contains(_)).headOption.map(_._2)
          (supported, response.content) match{
            case (Some(compression), Some(content)) => response.copy(content = Some(compression(content)))
            case _ => response
          }
        }
      }
    }

    val metadata = None
  }

  object CompressService{
    val supportedCompressions = Map[Encoding, CompressedByteChunk](Encodings.gzip -> GZIPByteChunk, Encodings.deflate -> ZLIBByteChunk)
  }

  case class AggregateService(chunkSize: Option[DataSize], delegate: HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]]) extends DelegatingService[ByteChunk, Future[HttpResponse[ByteChunk]], ByteChunk, Future[HttpResponse[ByteChunk]]]{
    def service = {r: HttpRequest[ByteChunk] => r.content match {
      case Some(chunk) =>
        val result           = new Future[HttpResponse[ByteChunk]]()
        val aggregatedFuture = AggregatedByteChunk(chunk, chunkSize)
        aggregatedFuture.deliverTo{aggregated =>
          delegate.service(r.copy(content = Some(aggregated))).foreach{ f =>
            f.deliverTo(response => result.deliver(response))
            f.ifCanceled(th => result.cancel(th))
            result.ifCanceled(th => f.cancel(th))
          }
        }
        aggregatedFuture.ifCanceled(th => result.cancel(th))
        result.ifCanceled(th => aggregatedFuture.cancel(th))
        Success(result)
      case None        => delegate.service(r)
    }}

    lazy val metadata = Some(DataSizeMetadata(chunkSize))
  }

  case class JsonpService[T](delegate: HttpService[JValue, Future[HttpResponse[JValue]]])(implicit b1: Bijection[T, JValue], bstr: Bijection[T, String]) extends DelegatingService[T, Future[HttpResponse[T]], JValue, Future[HttpResponse[JValue]]]{
    private implicit val b2 = b1.inverse
    def service = {r: HttpRequest[T] => convertRequest(r).flatMap(delegate.service(_)).map(_.map(convertResponse(_, r.parameters.get('callback)))) }

    private def convertRequest(r: HttpRequest[T]): Validation[NotServed, HttpRequest[JValue]] = {
      import blueeyes.json.JsonParser.parse
      import blueeyes.json.xschema.DefaultSerialization._

      r.parameters.get('callback) match {
        case Some(callback) if (r.method == HttpMethods.GET) =>
          if (r.content.isEmpty){
            val methodStr = r.parameters.get('method).getOrElse("get").toUpperCase

            val method  = HttpMethods.PredefinedHttpMethods.find(_.value == methodStr).getOrElse(HttpMethods.GET)
            val content = r.parameters.get('content).map(parse _)
            val headers = r.parameters.get('headers).map(parse _).map(_.deserialize[Map[String, String]]).getOrElse(Map.empty[String, String])

            Success(r.copy(method = method, content = content, headers = r.headers ++ headers))
          }
          else Failure(DispatchError(HttpException(HttpStatusCodes.BadRequest, "JSONP requested but content body is non-empty")))

        case Some(callback) =>
          Failure(DispatchError(HttpException(HttpStatusCodes.BadRequest, "JSONP requested but HTTP method is not GET")))

        case None =>
          Success(r.copy(content = r.content.map(b1.apply)))
      }
    }

    private def convertResponse(r: HttpResponse[JValue], callback: Option[String]): HttpResponse[T] = {
      import blueeyes.json.xschema.DefaultSerialization._
      import blueeyes.json.Printer._

      (callback match {
        case Some(callback) =>
          val meta = compact(render(JObject(
            JField("headers", r.headers.raw.serialize) ::
            JField("status",
              JObject(
                JField("code",    r.status.code.value.serialize) ::
                JField("reason",  r.status.reason) ::
                Nil
              )
            ) ::
            Nil
          )))

          r.copy(content = r.content.map { content =>
            bstr.inverse.apply(callback + "(" + bstr.apply(b2.apply(content)) + "," + meta + ");")
          }.orElse {
            Some(
              bstr.inverse.apply(callback + "(undefined," + meta + ");")
            )
          }, headers = r.headers + `Content-Type`(MimeTypes.text/MimeTypes.javascript))

        case None =>
          r.copy(content = r.content.map(b2.apply), headers = r.headers + `Content-Type`(MimeTypes.application/MimeTypes.json))
      })
    }

    def metadata = None
  }

  case class ForwardingService[T, U](f: HttpRequest[T] => Option[HttpRequest[U]], httpClient: HttpClient[U], delegate: HttpService[T, Future[HttpResponse[T]]]) extends DelegatingService[T, Future[HttpResponse[T]], T, Future[HttpResponse[T]]]{
    def service = { r: HttpRequest[T] =>
      Future.async(f(r).foreach(httpClient))
      delegate.service(r)
    }

    def metadata = None
  }

  trait HttpRequestHandlerCombinators2{
    /** The path combinator creates a handler that is defined only for suffixes
     * of the specified path pattern.
     *
     * {{{
     * path("/foo") {
     *   ...
     * }
     * }}}
     */
    def path[T, S](path: RestPathPattern): HttpService[T, S] => HttpService[T, S] = (h: HttpService[T, S]) => new PathService[T, S] (path, h)

    /** Yields the remaining path to the specified function, which should return
     * a request handler.
     * {{{
     * remainingPath {
     *   get {
     *     ...
     *   }
     * }
     * }}}
     */
    def remainingPath[T, S](h: HttpService[T, S]) = path(RestPathPattern.Root `...` ('remainingPath))(h)

    /** The method combinator creates a handler that is defined only for the
     * specified HTTP method.
     */
    def method[T, S](method: HttpMethod): HttpService[T, S] => HttpService[T, S] = (h: HttpService[T, S]) => new HttpMethodService[T, S] (method, h)

    /**
     * <pre>
     * path("/foo") {
     *   ...
     * } ~ orFail { request => BadRequest -> "The path " + request.path + " is malformed" }
     * </pre>
     */
    def orFail[T, S](h: HttpRequest[T] => (HttpFailure, String)): HttpService[T, S] = new FailureService[T, S](h)

    /**
     * <pre>
     * path("/foo") {
     *   ...
     * } ~ orFail(HttpStatusCodes.NotFound, "No handler found that could handle this request.")
     * </pre>
     */
    def orFail[T, S](code: HttpFailure, msg: String): HttpService[T, S] = orFail { request => code -> msg }

    /**
     * <pre>
     * path("/foo") {
     *   ...
     * } ~ orFail("The path is malformed")
     * </pre>
     */
    def orFail[T, S](msg: String): HttpService[T, S] = orFail { request => HttpStatusCodes.BadRequest -> msg }

    /** The path end combinator creates a handler that is defined only for paths
     * that are fully matched.
     */
    def $ [T, S](h: HttpService[T, S]): HttpService[T, S] = path(RestPathPatternParsers.EmptyPathPattern) { h }

    /** Forces a particular combinator to match.
     * <pre>
     * commit(r => BadRequest -> "Bad path: " + r.path) {
     *   path("/foo") {
     *     ...
     *   }
     * }
     * </pre>
     */
    def commit[T, S](msgGen: HttpRequest[T] => (HttpFailure, String))(h: HttpService[T, S]): HttpService[T, S] = CommitService(msgGen, h)

    /** Converts a full request handler into a partial request handler that
     * handles every input. Note: This is an implicit and will automatically
     * convert all full request handlers into partial request handlers,
     * as required by type signatures.
     */
    implicit def commitFull[T, S](h: HttpRequest[T] => S)(metadata: => Option[Metadata]): HttpService[T, S] = HttpHandlerService(h, metadata)


    /** Attemps to peek to see if a particular handler will handle a request.
     * Used to convert a fast-failing handler into a skipping one.
     * <pre>
     * justTry {
     *   path("/foo") {
     *     ...
     *   }
     * }
     * </pre>
     */
    def justTry[T, S](h: HttpService[T, S]): HttpService[T, S] = new JustTryService[T, S] (h)

    def get     [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = get     (h, None)
    def put     [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = put     (h, None)
    def post    [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = post    (h, None)
    def delete  [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = delete  (h, None)
    def head    [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = head    (h, None)
    def patch   [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = patch   (h, None)
    def options [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = options (h, None)
    def trace   [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = trace   (h, None)
    def connect [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = connect (h, None)
    def get     [T, S](h: HttpRequestHandlerFull3[T, S], metadata: => Option[Metadata]): HttpService[T, S] = $ { method(HttpMethods.GET)     { commitFull { h }{metadata} } }
    def put     [T, S](h: HttpRequestHandlerFull3[T, S], metadata: => Option[Metadata]): HttpService[T, S] = $ { method(HttpMethods.PUT)     { commitFull { h }{metadata} } }
    def post    [T, S](h: HttpRequestHandlerFull3[T, S], metadata: => Option[Metadata]): HttpService[T, S] = $ { method(HttpMethods.POST)    { commitFull { h }{metadata} } }
    def delete  [T, S](h: HttpRequestHandlerFull3[T, S], metadata: => Option[Metadata]): HttpService[T, S] = $ { method(HttpMethods.DELETE)  { commitFull { h }{metadata} } }
    def head    [T, S](h: HttpRequestHandlerFull3[T, S], metadata: => Option[Metadata]): HttpService[T, S] = $ { method(HttpMethods.HEAD)    { commitFull { h }{metadata} } }
    def patch   [T, S](h: HttpRequestHandlerFull3[T, S], metadata: => Option[Metadata]): HttpService[T, S] = $ { method(HttpMethods.PATCH)   { commitFull { h }{metadata} } }
    def options [T, S](h: HttpRequestHandlerFull3[T, S], metadata: => Option[Metadata]): HttpService[T, S] = $ { method(HttpMethods.OPTIONS) { commitFull { h }{metadata} } }
    def trace   [T, S](h: HttpRequestHandlerFull3[T, S], metadata: => Option[Metadata]): HttpService[T, S] = $ { method(HttpMethods.TRACE)   { commitFull { h }{metadata} } }
    def connect [T, S](h: HttpRequestHandlerFull3[T, S], metadata: => Option[Metadata]): HttpService[T, S] = $ { method(HttpMethods.CONNECT) { commitFull { h }{metadata} } }

    /** Creates a handler that accepts ranged GET requests. A ranged GET request
     * uses the Range header with the following syntax: [unit]=[lower-bound]-[upper-bound].
     * For example, bytes=0-123, indices=0-23.
     * {{{
     * getRange { (ranges, unit) =>
     *   (unit, ranges) match {
     *     case ("indices", (lowerBound, upperBound) :: Nil) =>
     *       // Retrieve all elements from lowerBound to upperBound
     *   }
     * }
     * }}}
     */
    def getRange[T, S](h: (List[(Long, Long)], String) => HttpRequestHandlerFull3[T, S]): HttpService[T, S] = getRange(h, None)
    def getRange[T, S](h: (List[(Long, Long)], String) => HttpRequestHandlerFull3[T, S], metadata: => Option[Metadata]): HttpService[T, S] = method(HttpMethods.GET)(GetRangeService(h, None))

    /** The accept combinator creates a handler that is defined only for requests
     * that have the specified content type. Requires an implicit bijection
     * used for transcoding.
     */
    def accept[T, S, U](mimeType: MimeType)(h: HttpService[T, S])(implicit b: Bijection[U, T]): HttpService[U, S] = new AcceptService[T, S, U](mimeType, h)

    /** The produce combinator creates a handler that is produces responses
     * that have the specified content type. Requires an implicit bijection
     * used for transcoding.
     */
    def produce[T, S, V](mimeType: MimeType)(h: HttpService[T, Future[HttpResponse[S]]])(implicit b: Bijection[S, V]): HttpService[T, Future[HttpResponse[V]]] = new ProduceService(mimeType, h)
    /** The content type combinator creates a handler that accepts and produces
     * requests and responses of the specified content type. Requires an implicit
     * bijection used for transcoding.
     */
    def contentType[T, S](mimeType: MimeType)(h: HttpService[T, Future[HttpResponse[T]]])(implicit b1: Bijection[S, T]): HttpService[S, Future[HttpResponse[S]]] = {
      implicit val b2 = b1.inverse

      accept(mimeType) {
        produce(mimeType) {
          h
        }
      }
    }

    /**
     *  The compress combinator creates a handler that compresses content by encoding supported by client
     *  (specified by Accept-Encoding header). The combinator supports gzip and deflate encoding.
     */
    def compress(h: HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]]): HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]] = new CompressService(h)

    /** The aggregate combinator creates a handler that stitches together chunks
     * to make a bigger chunk, up to the specified size.
     */
    def aggregate(chunkSize: Option[DataSize])(h: HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]]): HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]] = new AggregateService(chunkSize, h)

    /** The jsonp combinator creates a handler that accepts and produces JSON.
     * The handler also transparently works with JSONP, if the client specifies
     * a "callback" parameter in the query string. Clients may encode both
     * HTTP method and content using the query string parameters "method" and
     * "content", respectively.
     */
    def jsonp[T](delegate: HttpService[JValue, Future[HttpResponse[JValue]]])(implicit b1: Bijection[T, JValue], bstr: Bijection[T, String]): HttpService[T, Future[HttpResponse[T]]] = JsonpService[T](delegate)

    /** The jvalue combinator creates a handler that accepts and produces JSON.
     * Requires an implicit bijection used for transcoding.
     */
    def jvalue[T](h: HttpService[JValue, Future[HttpResponse[JValue]]])(implicit b: Bijection[T, JValue]): HttpService[T, Future[HttpResponse[T]]] = contentType(MimeTypes.application/MimeTypes.json) { h }

    /** The xml combinator creates a handler that accepts and produces XML.
     * Requires an implicit bijection used for transcoding.
     */
    def xml[T](h: HttpService[NodeSeq, Future[HttpResponse[NodeSeq]]])(implicit b: Bijection[T, NodeSeq]): HttpService[T, Future[HttpResponse[T]]] = contentType(MimeTypes.text/MimeTypes.xml) { h }

    def forwarding[T, U](f: HttpRequest[T] => Option[HttpRequest[U]], httpClient: HttpClient[U]) = (h: HttpService[T, Future[HttpResponse[T]]]) => new ForwardingService[T, U](f, httpClient, h)
  }

  def cookie[T](request: HttpRequest[T], s: Symbol): Option[String] = {
    def cookies = (for (HttpHeaders.Cookie(value) <- request.headers.raw) yield HttpHeaders.Cookie(value)).headOption.getOrElse(HttpHeaders.Cookie(Nil))
    cookies.cookies.find(_.name == s.name).map(_.cookieValue)
  }

  def field[S, F1 <: JValue](request: HttpRequest[JValue], s: Symbol)(implicit mc1: Manifest[F1]): Option[F1] = {
    val content = request.content.getOrElse(sys.error("Expected request body to be JSON object"))
    val c: Class[F1] = mc1.erasure.asInstanceOf[Class[F1]]

    ((content \ s.name) -->? c).asInstanceOf[Option[F1]]
  }
}

//object TestComb extends HttpServices.HttpRequestHandlerCombinators2 with RestPathPatternImplicits{
//  import blueeyes.core.data.ByteMemoryChunk
//  def main(args: Array[String]){
//    val value = service.service
//    println(value)
//  }
//  val service = path("foo"){
////    parameter[String, String](IdentifierWithDefault('bar, () => "foo")) { bar =>
//    compress{
//      get{ request: HttpRequest[ByteChunk] =>
//        Future.sync(HttpResponse[ByteChunk](content = Some(new ByteMemoryChunk(Array[Byte](), () => None))))
//      }
//    }
////    }
//  }
//}§