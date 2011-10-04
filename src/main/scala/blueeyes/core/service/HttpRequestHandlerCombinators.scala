package blueeyes
package core.service

import blueeyes.concurrent.Future
import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.data.{ByteChunk, Bijection}
import blueeyes.json.JsonAST._
import blueeyes.util.metrics.DataSize

import scala.xml.NodeSeq
import scalaz.Functor
import core.service.HttpServices._
import core.service.HttpServices.{HttpService => Service}

trait HttpRequestHandlerCombinators{
  /** The path combinator creates a handler that is defined only for suffixes
   * of the specified path pattern.
   *
   * {{{
   * path("/foo") {
   *   ...
   * }
   * }}}
   */
  def path[T, S](path: RestPathPattern): Service[T, S] => Service[T, S] = (h: Service[T, S]) => new PathService[T, S] (path, h)

  /** Yields the remaining path to the specified function, which should return
   * a request handler.
   * {{{
   * remainingPath {
   *   get {  (request: HttpRequest[T]) => { path =>
   *     ...
   *   }}
   * }
   * }}}
   */
  def remainingPath[T, S](handler: Service[T, String => S]) = path(RestPathPattern.Root `...` ('remainingPath)) {
    parameter(IdentifierWithDefault('remainingPath, () => ""))(handler)
  }

  /** The method combinator creates a handler that is defined only for the
   * specified HTTP method.
   */
  def method[T, S](method: HttpMethod): Service[T, S] => Service[T, S] = (h: Service[T, S]) => new HttpMethodService[T, S] (method, h)

  /**
   * <pre>
   * path("/foo") {
   *   ...
   * } ~ orFail { request => BadRequest -> "The path " + request.path + " is malformed" }
   * </pre>
   */
  def orFail[T, S](h: HttpRequest[T] => (HttpFailure, String)): Service[T, S] = new FailureService[T, S](h)

  /**
   * <pre>
   * path("/foo") {
   *   ...
   * } ~ orFail(HttpStatusCodes.NotFound, "No handler found that could handle this request.")
   * </pre>
   */
  def orFail[T, S](code: HttpFailure, msg: String): Service[T, S] = orFail { request => code -> msg }

  /**
   * <pre>
   * path("/foo") {
   *   ...
   * } ~ orFail("The path is malformed")
   * </pre>
   */
  def orFail[T, S](msg: String): Service[T, S] = orFail { request => HttpStatusCodes.BadRequest -> msg }

  /** The path end combinator creates a handler that is defined only for paths
   * that are fully matched.
   */
  def $ [T, S](h: Service[T, S]): Service[T, S] = path(RestPathPatternParsers.EmptyPathPattern) { h }

  /** Forces a particular combinator to match.
   * <pre>
   * commit(r => BadRequest -> "Bad path: " + r.path) {
   *   path("/foo") {
   *     ...
   *   }
   * }
   * </pre>
   */
  def commit[T, S](msgGen: HttpRequest[T] => (HttpFailure, String))(h: Service[T, S]): Service[T, S] = CommitService(msgGen, h)

  def get     [T, S](h: HttpServiceHandler[T, S]): Service[T, S] = $ { method(HttpMethods.GET)     { HttpHandlerService { h } } }
  def put     [T, S](h: HttpServiceHandler[T, S]): Service[T, S] = $ { method(HttpMethods.PUT)     { HttpHandlerService { h } } }
  def post    [T, S](h: HttpServiceHandler[T, S]): Service[T, S] = $ { method(HttpMethods.POST)    { HttpHandlerService { h } } }
  def delete  [T, S](h: HttpServiceHandler[T, S]): Service[T, S] = $ { method(HttpMethods.DELETE)  { HttpHandlerService { h } } }
  def head    [T, S](h: HttpServiceHandler[T, S]): Service[T, S] = $ { method(HttpMethods.HEAD)    { HttpHandlerService { h } } }
  def patch   [T, S](h: HttpServiceHandler[T, S]): Service[T, S] = $ { method(HttpMethods.PATCH)   { HttpHandlerService { h } } }
  def options [T, S](h: HttpServiceHandler[T, S]): Service[T, S] = $ { method(HttpMethods.OPTIONS) { HttpHandlerService { h } } }
  def trace   [T, S](h: HttpServiceHandler[T, S]): Service[T, S] = $ { method(HttpMethods.TRACE)   { HttpHandlerService { h } } }
  def connect [T, S](h: HttpServiceHandler[T, S]): Service[T, S] = $ { method(HttpMethods.CONNECT) { HttpHandlerService { h } } }

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
  def getRange[T, S](h: (List[(Long, Long)], String) => HttpServiceHandler[T, S]): Service[T, S] = method(HttpMethods.GET)(GetRangeService(h))

  /**
   * Extracts data from the request. The extractor combinators can be used to
   * factor out extraction logic that's duplicated across a range of handlers.
   * <p>
   * Extractors are fail-fast combinators. If they cannot extract the required
   * information during evaluation of isDefinedAt() method, they immediately
   * throw an HttpException.
   * <pre>
   * extract(_.parameters('username)) {
   *   get {  (request: HttpRequest[T]) => {username =>
   *     ...
   *   }}
   * }
   * </pre>
   */
  def extract[T, S, E1](s1AndDefault: IdentifierWithDefault[Symbol, E1])(extractor: HttpRequest[T] => E1): Service[T, E1 => S] => Service[T, S] = (h) => new ExtractService[T, S, E1](s1AndDefault, extractor, h)

  /** A special-case extractor for parameters.
   * <pre>
   * parameter('token) {
   *   get {  (request: HttpRequest[T]) => {token =>
   *     ...
   *   }}
   * }
   * </pre>
   */
  def parameter[T, S](s1AndDefault: IdentifierWithDefault[Symbol, String]): Service[T, String => S] => Service[T, S] = (h) => new ParameterService[T, S](s1AndDefault, h)

  private def extractCookie[T](request: HttpRequest[T], s: Symbol, defaultValue: Option[String] = None) = {
    def cookies = (for (HttpHeaders.Cookie(value) <- request.headers.raw) yield HttpHeaders.Cookie(value)).headOption.getOrElse(HttpHeaders.Cookie(Nil))
    cookies.cookies.find(_.name == s.name).map(_.cookieValue).orElse(defaultValue).getOrElse(sys.error("Expected cookie " + s.name))
  }
  /** A special-case extractor for cookie supporting a default value.
   * <pre>
   * cookie('token, "defaultValue") {  =>
   *   get {  (request: HttpRequest[T]) => { token =>
   *     ...
   *   }}
   * }
   * </pre>
   */
  def cookie[T, S](s1AndDefault: IdentifierWithDefault[Symbol, String]): Service[T, String => S] => Service[T, S] = extract[T, S, String] (s1AndDefault){ request =>
    extractCookie(request, s1AndDefault.identifier, Some(s1AndDefault.default))
  }

  def field[S, F1 <: JValue](s1AndDefault: IdentifierWithDefault[Symbol, F1])(implicit mc1: Manifest[F1]): Service[JValue, F1 => S] => Service[JValue, S] = {
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
  def accept[T, S, U](mimeType: MimeType)(h: Service[Future[T], S])(implicit b: Bijection[U, Future[T]]): Service[U, S] = new AcceptService[T, S, U](mimeType, h)

  /** The produce combinator creates a handler that is produces responses
   * that have the specified content type. Requires an implicit bijection
   * used for transcoding.
   */
  def produce[T, S, V](mimeType: MimeType)(h: Service[T, Future[HttpResponse[S]]])(implicit b: Bijection[S, V]): Service[T, Future[HttpResponse[V]]] = new ProduceService(mimeType, h)
  /** The content type combinator creates a handler that accepts and produces
   * requests and responses of the specified content type. Requires an implicit
   * bijection used for transcoding.
   */
  def contentType[T, S](mimeType: MimeType)(h: Service[Future[T], Future[HttpResponse[T]]])(implicit b1: Bijection[S, Future[T]], b2: Bijection[T, S]): Service[S, Future[HttpResponse[S]]] = {
    implicit val b3 = b2.inverse

    accept(mimeType) {
      produce[Future[T], T, S](mimeType) {
        h
      }
    }
  }

  /**
   *  The compress combinator creates a handler that compresses content by encoding supported by client
   *  (specified by Accept-Encoding header). The combinator supports gzip and deflate encoding.
   */
  def compress(h: Service[ByteChunk, Future[HttpResponse[ByteChunk]]]): Service[ByteChunk, Future[HttpResponse[ByteChunk]]] = new CompressService(h)

  /** The aggregate combinator creates a handler that stitches together chunks
   * to make a bigger chunk, up to the specified size.
   */
  def aggregate(chunkSize: Option[DataSize])(h: Service[Future[ByteChunk], Future[HttpResponse[ByteChunk]]]): Service[ByteChunk, Future[HttpResponse[ByteChunk]]] = new AggregateService(chunkSize, h)

  /** The jsonp combinator creates a handler that accepts and produces JSON.
   * The handler also transparently works with JSONP, if the client specifies
   * a "callback" parameter in the query string. Clients may encode both
   * HTTP method and content using the query string parameters "method" and
   * "content", respectively.
   */
  def jsonp[T](delegate: Service[JValue, Future[HttpResponse[JValue]]])(implicit b1: Bijection[T, JValue], bstr: Bijection[T, String]): Service[T, Future[HttpResponse[T]]] = JsonpService[T](delegate)

  /** The jvalue combinator creates a handler that accepts and produces JSON.
   * Requires an implicit bijection used for transcoding.
   */
  def jvalue[T](h: Service[Future[JValue], Future[HttpResponse[JValue]]])(implicit b1: Bijection[T, Future[JValue]], b2: Bijection[JValue, T]): Service[T, Future[HttpResponse[T]]] = contentType(MimeTypes.application/MimeTypes.json) { h }

  /** The xml combinator creates a handler that accepts and produces XML.
   * Requires an implicit bijection used for transcoding.
   */
  def xml[T](h: Service[Future[NodeSeq], Future[HttpResponse[NodeSeq]]])(implicit b1: Bijection[T, Future[NodeSeq]], b2: Bijection[NodeSeq, T]): Service[T, Future[HttpResponse[T]]] = contentType(MimeTypes.text/MimeTypes.xml) { h }

  def forwarding[T, U](f: HttpRequest[T] => Option[HttpRequest[U]], httpClient: HttpClient[U]) = (h: Service[T, HttpResponse[T]]) => new ForwardingService[T, U](f, httpClient, h)

  def metadata[T, S](metadata: Metadata*)(h: Service[T, HttpResponse[S]]) = MetadataService(Some(CompositeMetadata(metadata)), h)

  def describe[T, S](description: String)(h: Service[T, HttpResponse[S]]) = MetadataService(Some(DescriptionMetadata(description)), h)

  /** The decodeUrl combinator creates a handler that decode HttpRequest URI.
   */
  def decodeUrl[T, S](h: Service[T, S]) = new DecodeUrlService[T, S](h)
}


case class IdentifierWithDefault[T, S](identifier: T, default_ : () => S) {
  def default = default_ ()
}
