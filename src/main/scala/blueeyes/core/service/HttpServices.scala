package blueeyes.core.service

import scalaz.{Success, Validation, Failure}
import scalaz.Scalaz._
import blueeyes.json.JsonAST.JValue
import blueeyes.core.http._
import annotation.tailrec
import java.net.URLDecoder._
import blueeyes.core.data.Bijection
import blueeyes.core.http.HttpHeaders.`Content-Type`
import blueeyes.concurrent.Future

object HttpServices{
  sealed trait NotServed {
    def or[A](result: => Validation[NotServed, A]): Validation[NotServed, A]
  }

  case class ServiceError(exception: HttpException) extends NotServed {
    override def or[A](result: => Validation[NotServed, A]) = this.fail[A]
  }

  case object Inapplicable extends NotServed {
    override def or[A](result: => Validation[NotServed, A]) = result
  }

  sealed trait Metadata
  case class OrMetadata(docs: List[Metadata])              extends Metadata
  case object FailureMetadata                              extends Metadata

  case class ContentMetadata(mimeType: MimeType)                              extends Metadata
  case class PathPatternMetadata(pattern: RestPathPattern)                    extends Metadata
  case class HttpMethodMetadata(method: HttpMethod)                           extends Metadata
  case class ParameterMetadata[T, S](parameter: IdentifierWithDefault[T, S])  extends Metadata
  case class DescriptionMetadata(description: String)                         extends Metadata

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

    lazy val metadata = Some(OrMetadata(services.map(_.metadata).collect{case Some(x) => x}.toList))
  }

  trait DelegatingService[A, B, C, D] extends HttpService[A, B] {
    val delegate: HttpService[C, D]
  }

  trait ParameterBasedService[A, B, C, D, P] extends HttpService[A, B] {
    val delegate: P => HttpService[C, D]

    val default: P
  }

  case class ParameterService[A, B](s1AndDefault: IdentifierWithDefault[Symbol, String], delegate: String => HttpService[A, B]) extends ParameterBasedService[A, B, A, B, String]{
    def service = {r: HttpRequest[A] =>
      val value = extract(r)

      delegate(value).service(addParameter(r, (s1AndDefault.identifier -> value)))
    }

    val default = s1AndDefault.default

    lazy val metadata = Some(ParameterMetadata(s1AndDefault))

    private def addParameter(r: HttpRequest[A], newParam: (Symbol, String)): HttpRequest[A] = r.copy(parameters = r.parameters + newParam)

    private def extract(r: HttpRequest[A]): String = r.parameters.get(s1AndDefault.identifier).getOrElse(s1AndDefault.default)
  }

  case class ExtractService[A, B, P](s1AndDefault: IdentifierWithDefault[Symbol, P], extractor: HttpRequest[A] => P, delegate: P => HttpService[A, B]) extends ParameterBasedService[A, B, A, B, P]{
    def service = {r: HttpRequest[A] =>
      try {
        val extracted = extractor(r)

        delegate(extracted).service(r)
      }
      catch {
        case t: Throwable => Inapplicable.fail
      }
    }

    val default = s1AndDefault.default

    lazy val metadata = Some(ParameterMetadata(s1AndDefault))
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
    val service = (r: HttpRequest[A]) => Failure(ServiceError(onFailure(r).fold(HttpException(_, _))))

    val metadata = None
  }

  case class CommitService[A, B](onFailure: HttpRequest[A] => (HttpFailure, String), delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
    val service = (r: HttpRequest[A]) => delegate.service(r) match {
      case Failure(Inapplicable) => Failure(ServiceError(onFailure(r).fold(HttpException(_, _))))
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

      def handle(r: HttpRequest[U]) = newContent(r) match{
        case Failure(failure) => Failure(failure)
        case Success(content) => delegate.service(r.copy(content = content))
      }

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
    def remainingPath[T, S](handler: String => HttpService[T, S]) = path(RestPathPattern.Root `...` ('remainingPath)) {
      parameter(IdentifierWithDefault('remainingPath, () => "")) {
        handler
      }
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

    /**
     * Extracts data from the request. The extractor combinators can be used to
     * factor out extraction logic that's duplicated across a range of handlers.
     * <p>
     * Extractors are fail-fast combinators. If they cannot extract the required
     * information during evaluation of isDefinedAt() method, they immediately
     * throw an HttpException.
     * <pre>
     * extract("foo").(_.parameters('username)) { username =>
     *   ...
     * }
     * </pre>
     */
    def extract[T, S, E1](s1AndDefault: IdentifierWithDefault[Symbol, E1])(extractor: HttpRequest[T] => E1) = (h: E1 => HttpService[T, S]) => new ExtractService[T, S, E1](s1AndDefault, extractor, h)

    /** A special-case extractor for parameters.
     * <pre>
     * parameter('token) { token =>
     *   get {
     *     ...
     *   }
     * }
     * </pre>
     */
    def parameter[T, S](s1AndDefault: IdentifierWithDefault[Symbol, String]) = (h: String => HttpService[T, S]) => new ParameterService[T, S](s1AndDefault, h)

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
    def cookie[T, S](s1AndDefault: IdentifierWithDefault[Symbol, String])(h: String => HttpService[T, S]): HttpService[T, S] = extract[T, S, String] (s1AndDefault){ request =>
      extractCookie(request, s1AndDefault.identifier, Some(s1AndDefault.default))
    } { h }

    def field[S, F1 <: JValue](s1AndDefault: IdentifierWithDefault[Symbol, F1])(implicit mc1: Manifest[F1]) = (h: F1 => HttpService[JValue, S]) => {
      def extractField[F <: JValue](content: JValue, s1AndDefault: IdentifierWithDefault[Symbol, F])(implicit mc: Manifest[F]): F = {
        val c: Class[F] = mc.erasure.asInstanceOf[Class[F]]

        ((content \ s1AndDefault.identifier.name) -->? c).getOrElse(s1AndDefault.default).asInstanceOf[F]
      }

      extract[JValue, S, F1](s1AndDefault) { (request: HttpRequest[JValue]) =>
        val content = request.content.getOrElse(sys.error("Expected request body to be JSON object"))

        extractField(content, s1AndDefault)
      } (h)
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
    def produce[T, S, V](mimeType: MimeType)(h: HttpService[T, Future[HttpResponse[S]]])(implicit b: Bijection[S, V]): HttpService[T, Future[HttpResponse[V]]] = new ProduceService(mimeType, h)
    /** The content type combinator creates a handler that accepts and produces
     * requests and responses of the specified content type. Requires an implicit
     * bijection used for transcoding.
     */
//    def contentType[T, S](mimeType: MimeType)(h: HttpService[T, T])(implicit b1: Bijection[S, T]): HttpService[S, S] = {
//      implicit val b2 = b1.inverse
//
//      accept(mimeType) {
//        produce(mimeType) {
//          h
//        }
//      }
//    }
  }
}

object TestComb extends HttpServices.HttpRequestHandlerCombinators2 with RestPathPatternImplicits{
  def main(args: Array[String]){
    val value = service.service
    println(value)
  }
  val service = path("foo"){
    parameter[String, String](IdentifierWithDefault('bar, () => "foo")) { bar =>
      get{ request: HttpRequest[String] =>
        bar + "foo"
      }
    }
  }
}