package blueeyes.core.service

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
import scalaz.{Functor, Success, Validation, Failure}

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

  case class DataSizeMetadata    (dataSize: Option[DataSize])             extends Metadata
  case class HeaderMetadata       (mimeType: HttpHeader)                  extends Metadata
  case class PathPatternMetadata  (pattern: RestPathPattern)              extends Metadata
  case class HttpMethodMetadata   (method: HttpMethod)                    extends Metadata
  case class DescriptionMetadata  (description: String)                   extends Metadata
  case class ExtractMetadata[T, S](extract: IdentifierWithDefault[T, S])  extends Metadata

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

  case class HttpHandlerService[A, B](h: HttpRequestHandlerFull3[A, B]) extends HttpService[A, B]{
    val service = (r: HttpRequest[A]) => success(h(r))

    def metadata = None
  }

  case class FailureService[A, B](onFailure: HttpRequest[A] => (HttpFailure, String)) extends HttpService[A, B]{
    val service = (r: HttpRequest[A]) => failure(DispatchError(onFailure(r).fold(HttpException(_, _))))

    val metadata = None
  }

  case class CommitService[A, B](onFailure: HttpRequest[A] => (HttpFailure, String), delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
    val service = (r: HttpRequest[A]) => delegate.service(r) match {
      case Failure(Inapplicable) => failure(DispatchError(onFailure(r).fold(HttpException(_, _))))
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
        success(r.content.map(b.apply _))
      }
      catch {
        case ex => Inapplicable.fail
      }

      def handle(r: HttpRequest[U]) = newContent(r).flatMap(content => delegate.service(r.copy(content = content)))

      r.mimeTypes.find(_ == mimeType).map { mimeType => handle(r) }.orElse { r.content.map(v => handle(r)) }.getOrElse(Inapplicable.fail)
    }

    lazy val metadata = Some(HeaderMetadata(`Content-Type`(mimeType)))
  }

  case class GetRangeService[T, S](h: (List[(Long, Long)], String) => HttpRequestHandlerFull3[T, S]) extends HttpService[T, S]{
    def service = { r => extractRange(r.headers.raw).map(range => h.tupled(range)(r)) }

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
                success[NotServed, (List[(Long, Long)], String)]((pureRanges, unit.trim.toLowerCase))
              }
            case _ => Inapplicable.fail
          }
      }.headOption.getOrElse(Inapplicable.fail)
    }

    def metadata = None
  }

  case class ProduceService[T, S, V, M[_]](mimeType: MimeType, delegate: HttpService[T, M[HttpResponse[S]]])(implicit b: Bijection[S, V], functor: Functor[M]) extends DelegatingService[T, M[HttpResponse[V]], T, M[HttpResponse[S]]]{
    def service = (r: HttpRequest[T]) => delegate.service(r).map { f: M[HttpResponse[S]] =>
        f.map{response => response.copy(content = response.content.map(b.apply), headers = response.headers + `Content-Type`(mimeType))
      }
    }

    lazy val metadata = Some(HeaderMetadata(`Content-Type`(mimeType)))
  }

  case class CompressService[M[_]:Functor](delegate: HttpService[ByteChunk, M[HttpResponse[ByteChunk]]]) extends DelegatingService[ByteChunk, M[HttpResponse[ByteChunk]], ByteChunk, M[HttpResponse[ByteChunk]]]{
    def service = (r: HttpRequest[ByteChunk]) => delegate.service(r).map{ma =>
      ma.map{response =>
      val encodings  = r.headers.header(`Accept-Encoding`).map(_.encodings.toList).getOrElse(Nil)
      val supported  = CompressService.supportedCompressions.filterKeys(encodings.contains(_)).headOption.map(_._2)
      (supported, response.content) match{
        case (Some(compression), Some(content)) => response.copy(content = Some(compression(content)))
        case _ => response
      }
    }}

    val metadata = None
  }

  object CompressService{
    val supportedCompressions = Map[Encoding, CompressedByteChunk](Encodings.gzip -> GZIPByteChunk, Encodings.deflate -> ZLIBByteChunk)
  }

  case class AggregateService(chunkSize: Option[DataSize], delegate: HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]]) extends DelegatingService[ByteChunk, Future[HttpResponse[ByteChunk]], ByteChunk, Future[HttpResponse[ByteChunk]]]{
    def service = (r: HttpRequest[ByteChunk]) => r.content match {
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
        success(result)
      case None        => delegate.service(r)
    }

    lazy val metadata = Some(DataSizeMetadata(chunkSize))
  }

  case class JsonpService[T, M[_]](delegate: HttpService[JValue, M[HttpResponse[JValue]]])(implicit b1: Bijection[T, JValue], bstr: Bijection[T, String], functor: Functor[M]) extends DelegatingService[T, M[HttpResponse[T]], JValue, M[HttpResponse[JValue]]]{
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

            success(r.copy(method = method, content = content, headers = r.headers ++ headers))
          }
          else failure(DispatchError(HttpException(HttpStatusCodes.BadRequest, "JSONP requested but content body is non-empty")))

        case Some(callback) =>
          failure(DispatchError(HttpException(HttpStatusCodes.BadRequest, "JSONP requested but HTTP method is not GET")))

        case None =>
          success(r.copy(content = r.content.map(b1.apply)))
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

  case class ForwardingService[T, U](f: HttpRequest[T] => Option[HttpRequest[U]], httpClient: HttpClient[U], delegate: HttpService[T, HttpResponse[T]]) extends DelegatingService[T, HttpResponse[T], T, HttpResponse[T]]{
    def service = { r: HttpRequest[T] =>
      Future.async(f(r).foreach(httpClient))
      delegate.service(r)
    }

    def metadata = None
  }

  case class ParameterService[T, S](s1AndDefault: IdentifierWithDefault[Symbol, String], delegate: HttpService[T, String => S]) extends DelegatingService[T, S, T, String => S]{
    def service = {r: HttpRequest[T] =>
      val parameter  = extract(r)
      val newRequest = addParameter(r, (s1AndDefault.identifier, parameter))
      delegate.service(newRequest).map(_.apply(parameter))
    }

    private def addParameter(r: HttpRequest[T], newParam: (Symbol, String)): HttpRequest[T] = r.copy(parameters = r.parameters + newParam)

    private def extract(r: HttpRequest[T]): String = r.parameters.get(s1AndDefault.identifier).getOrElse(s1AndDefault.default)

    lazy val metadata = Some(ExtractMetadata(s1AndDefault))
  }

  case class ExtractService[T, S, P](s1AndDefault: IdentifierWithDefault[Symbol, P], extractor: HttpRequest[T] => P, delegate: HttpService[T, P => S]) extends DelegatingService[T, S, T, P => S]{
    def service = {r: HttpRequest[T] =>
      try {
        delegate.service(r).map(_.apply(extractor(r)))
      }
      catch {
        case t: Throwable => Inapplicable.fail
      }
    }

    lazy val metadata = Some(ExtractMetadata(s1AndDefault))
  }

  case class MetadataService[T, S](metadata: Option[Metadata], delegate: HttpService[T, S]) extends DelegatingService[T, S, T, S]{
    def service = delegate.service
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
     * remainingPath { path =>
     *   get {
     *     ...
     *   }
     * }
     * }}}
     */
    def remainingPath[T, S](handler: HttpService[T, String => S]) = path(RestPathPattern.Root `...` ('remainingPath)) {
      parameter(IdentifierWithDefault('remainingPath, () => ""))(handler)
    }

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
    implicit def commitFull[T, S](h: HttpRequest[T] => S): HttpService[T, S] = HttpHandlerService(h)


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

    def get     [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = $ { method(HttpMethods.GET)     { commitFull { h } } }
    def put     [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = $ { method(HttpMethods.PUT)     { commitFull { h } } }
    def post    [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = $ { method(HttpMethods.POST)    { commitFull { h } } }
    def delete  [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = $ { method(HttpMethods.DELETE)  { commitFull { h } } }
    def head    [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = $ { method(HttpMethods.HEAD)    { commitFull { h } } }
    def patch   [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = $ { method(HttpMethods.PATCH)   { commitFull { h } } }
    def options [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = $ { method(HttpMethods.OPTIONS) { commitFull { h } } }
    def trace   [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = $ { method(HttpMethods.TRACE)   { commitFull { h } } }
    def connect [T, S](h: HttpRequestHandlerFull3[T, S]): HttpService[T, S] = $ { method(HttpMethods.CONNECT) { commitFull { h } } }

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
    def getRange[T, S](h: (List[(Long, Long)], String) => HttpRequestHandlerFull3[T, S]): HttpService[T, S] = method(HttpMethods.GET)(GetRangeService(h))

    /**
     * Extracts data from the request. The extractor combinators can be used to
     * factor out extraction logic that's duplicated across a range of handlers.
     * <p>
     * Extractors are fail-fast combinators. If they cannot extract the required
     * information during evaluation of isDefinedAt() method, they immediately
     * throw an HttpException.
     * <pre>
     * extract(_.parameters('username)) { username =>
     *   ...
     * }
     * </pre>
     */
    def extract[T, S, E1](s1AndDefault: IdentifierWithDefault[Symbol, E1])(extractor: HttpRequest[T] => E1): HttpService[T, E1 => S] => HttpService[T, S] = (h) => new ExtractService[T, S, E1](s1AndDefault, extractor, h)

    /** A special-case extractor for parameters.
     * <pre>
     * parameter('token) { token =>
     *   get {
     *     ...
     *   }
     * }
     * </pre>
     */
    def parameter[T, S](s1AndDefault: IdentifierWithDefault[Symbol, String]): HttpService[T, String => S] => HttpService[T, S] = (h) => new ParameterService[T, S](s1AndDefault, h)

    private def extractCookie[T](request: HttpRequest[T], s: Symbol, defaultValue: Option[String] = None) = {
      def cookies = (for (HttpHeaders.Cookie(value) <- request.headers.raw) yield HttpHeaders.Cookie(value)).headOption.getOrElse(HttpHeaders.Cookie(Nil))
      cookies.cookies.find(_.name == s.name).map(_.cookieValue).orElse(defaultValue).getOrElse(sys.error("Expected cookie " + s.name))
    }
    /** A special-case extractor for cookie supporting a default value.
     * <pre>
     * cookie('token, "defaultValue") { token =>
     *   get {
     *     ...
     *   }
     * }
     * </pre>
     */
    def cookie[T, S](s1AndDefault: IdentifierWithDefault[Symbol, String]): HttpService[T, String => S] => HttpService[T, S] = extract[T, S, String] (s1AndDefault){ request =>
      extractCookie(request, s1AndDefault.identifier, Some(s1AndDefault.default))
    }

    def field[S, F1 <: JValue](s1AndDefault: IdentifierWithDefault[Symbol, F1])(implicit mc1: Manifest[F1]): HttpService[JValue, F1 => S] => HttpService[JValue, S] = {
      def extractField[F <: JValue](content: JValue, s1AndDefault: IdentifierWithDefault[Symbol, F])(implicit mc: Manifest[F]): F = {
        val c: Class[F] = mc.erasure.asInstanceOf[Class[F]]

        ((content \ s1AndDefault.identifier.name) -->? c).getOrElse(s1AndDefault.default).asInstanceOf[F]
      }

      extract[JValue, S, F1](s1AndDefault) { (request: HttpRequest[JValue]) =>
        val content = request.content.getOrElse(sys.error("Expected request body to be JSON object"))

        extractField(content, s1AndDefault)
      }
    }

    /** The accept combinator creates a handler that is defined only for requests
     * that have the specified content type. Requires an implicit bijection
     * used for transcoding.
     */
    def accept[T, S, U](mimeType: MimeType)(h: HttpService[T, S])(implicit b: Bijection[U, T]): HttpService[U, S] = new AcceptService[T, S, U](mimeType, h)

    /** The produce combinator creates a handler that is produces responses
     * that have the specified content type. Requires an implicit bijection
     * used for transcoding.
     */
    def produce[T, S, V, M[_]](mimeType: MimeType)(h: HttpService[T, M[HttpResponse[S]]])(implicit b: Bijection[S, V], functor: Functor[M]): HttpService[T, M[HttpResponse[V]]] = new ProduceService(mimeType, h)
    /** The content type combinator creates a handler that accepts and produces
     * requests and responses of the specified content type. Requires an implicit
     * bijection used for transcoding.
     */
    def contentType[T, S, M[_]](mimeType: MimeType)(h: HttpService[T, M[HttpResponse[T]]])(implicit b1: Bijection[S, T], functor: Functor[M]): HttpService[S, M[HttpResponse[S]]] = {
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
    def compress[M[_]: Functor](h: HttpService[ByteChunk, M[HttpResponse[ByteChunk]]]): HttpService[ByteChunk, M[HttpResponse[ByteChunk]]] = new CompressService(h)

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
    def jsonp[T, M[_]](delegate: HttpService[JValue, M[HttpResponse[JValue]]])(implicit b1: Bijection[T, JValue], bstr: Bijection[T, String], functor: Functor[M]): HttpService[T, M[HttpResponse[T]]] = JsonpService[T, M](delegate)

    /** The jvalue combinator creates a handler that accepts and produces JSON.
     * Requires an implicit bijection used for transcoding.
     */
    def jvalue[T, M[_]](h: HttpService[JValue, M[HttpResponse[JValue]]])(implicit b: Bijection[T, JValue], functor: Functor[M]): HttpService[T, M[HttpResponse[T]]] = contentType(MimeTypes.application/MimeTypes.json) { h }

    /** The xml combinator creates a handler that accepts and produces XML.
     * Requires an implicit bijection used for transcoding.
     */
    def xml[T, M[_]](h: HttpService[NodeSeq, M[HttpResponse[NodeSeq]]])(implicit b: Bijection[T, NodeSeq], functor: Functor[M]): HttpService[T, M[HttpResponse[T]]] = contentType(MimeTypes.text/MimeTypes.xml) { h }

    def forwarding[T, U](f: HttpRequest[T] => Option[HttpRequest[U]], httpClient: HttpClient[U]) = (h: HttpService[T, HttpResponse[T]]) => new ForwardingService[T, U](f, httpClient, h)

    def metadata[T, S](metadata: Metadata)(h: HttpService[T, HttpResponse[S]]) = MetadataService(Some(metadata), h)

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
}

object TestComb extends HttpServices.HttpRequestHandlerCombinators2 with RestPathPatternImplicits{
  import blueeyes.concurrent.Future._
  import blueeyes.core.data.ByteMemoryChunk
  def main(args: Array[String]){
    val value = service.service
    println(value)
  }
  val service = path("foo"){
    compress{
      parameter(IdentifierWithDefault('bar, () => "foo")) {
        get{ request: HttpRequest[ByteChunk] => { param: String =>
            Future.sync(HttpResponse[ByteChunk](content = Some(new ByteMemoryChunk(Array[Byte](), () => None))))
          }
        }
      }
    }
  }
}