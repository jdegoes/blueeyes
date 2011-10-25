package blueeyes.core
package service

import http._
import http.HttpHeaders._
import http.HttpStatusCodes._
import data.{CompressedByteChunk, ByteChunk, Bijection}

import blueeyes.concurrent.Future
import blueeyes.json.JsonAST._
import blueeyes.util.metrics.DataSize

import scala.xml.NodeSeq
import scalaz.Scalaz._
import scalaz.{Success, Validation, Failure}


trait HttpRequestHandlerCombinators {
  implicit def service[A, B](handler: HttpServiceHandler[A, B]): HttpService[A, B] = HttpHandlerService[A, B](handler)

  /** The path combinator creates a handler that is defined only for suffixes
   * of the specified path pattern.
   *
   * {{{
   * path("/foo") {
   *   ...
   * }
   * }}}
   */
  def path[T, S](path: RestPathPattern, desc: Option[String] = None): HttpService[T, S] => HttpService[T, S] =
    (h: HttpService[T, S]) => new PathService[T, S] (path, h, desc)

  def pathParameter[A, B](path: RestPathPattern, sym: Symbol, desc: Option[String] = None) = 
    (h: HttpService[A, String => B]) => new PathParameterService(path, sym, desc, h)

  def pathData[A, B, C](path: RestPathPattern, sym: Symbol, f: String => Validation[NotServed, B], desc: Option[String] = None) = 
    (h: HttpService[A, B => C]) => new PathDataService(path, sym, f, desc, h)

  /** The path end combinator creates a handler that is defined only for paths
   * that are fully matched.
   */
  def $ [T, S] = path[T, S](RestPathPatternParsers.EmptyPathPattern)

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
  def remainingPath[T, S] = path(RestPathPattern.Root `...` ('remainingPath)) andThen parameter[T, S]('remainingPath, Some(""))

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
  def orFail[T, S](msg: String): HttpService[T, S] = orFail(BadRequest, msg)

  /** Forces a particular combinator to match.
   * <pre>
   * commit(r => BadRequest -> "Bad path: " + r.path) {
   *   path("/foo") {
   *     ...
   *   }
   * }
   * </pre>
   */
  def commit[T, S](delegate: HttpService[T, S]): HttpService[T, S] = CommitService(delegate)

  def get     [T, S](h: HttpService[T, S]): HttpService[T, S] = $ { method(HttpMethods.GET)     { h } }
  def put     [T, S](h: HttpService[T, S]): HttpService[T, S] = $ { method(HttpMethods.PUT)     { h } }
  def post    [T, S](h: HttpService[T, S]): HttpService[T, S] = $ { method(HttpMethods.POST)    { h } }
  def delete  [T, S](h: HttpService[T, S]): HttpService[T, S] = $ { method(HttpMethods.DELETE)  { h } }
  def head    [T, S](h: HttpService[T, S]): HttpService[T, S] = $ { method(HttpMethods.HEAD)    { h } }
  def patch   [T, S](h: HttpService[T, S]): HttpService[T, S] = $ { method(HttpMethods.PATCH)   { h } }
  def options [T, S](h: HttpService[T, S]): HttpService[T, S] = $ { method(HttpMethods.OPTIONS) { h } }
  def trace   [T, S](h: HttpService[T, S]): HttpService[T, S] = $ { method(HttpMethods.TRACE)   { h } }
  def connect [T, S](h: HttpService[T, S]): HttpService[T, S] = $ { method(HttpMethods.CONNECT) { h } }

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
  def getRange[T, S](delegate: HttpService[T, RangeHeaderValues => S]): HttpService[T, S] = method(HttpMethods.GET)(GetRangeService(delegate))

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
  def extract[T, S, E1](extractor: HttpRequest[T] => E1): HttpService[T, E1 => S] => HttpService[T, S] = ExtractService[T, S, E1](extractor, _)

  /** A special-case extractor for parameters.
   * <pre>
   * parameter('token) {
   *   get {  (request: HttpRequest[T]) => {token =>
   *     ...
   *   }}
   * }
   * </pre>
   */
  def parameter[T, S](parameter: Symbol, default: => Option[String] = None, desc: Option[String] = None) = {
    ParameterService[T, S](parameter, default, desc, _: HttpService[T, String => S])
  }

  def convertedParameter[T, S, E1](parameter: Symbol, convert: String => E1, default: => Option[String] = None, desc: Option[String] = None) = {
    (s: HttpService[T, E1 => S]) => ParameterService[T, S](parameter, default, desc, s.map((_: E1 => S) compose convert))
  }

  /** A special-case extractor for cookie supporting a default value.
   * <pre>
   * cookie('token, "defaultValue") {
   *   get {  (request: HttpRequest[T]) => { token =>
   *     ...
   *   }}
   * }
   * </pre>
   */
  def cookie[T, S](ident: Symbol, default: => Option[String] = None, desc: Option[String] = None) = {
    CookieService[T, S](ident, default, desc, _: HttpService[T, String => S])
  }

  def convertedCookie[T, S, E1](ident: Symbol, convert: String => E1, default: => Option[String] = None, desc: Option[String] = None) = {
    (s: HttpService[T, E1 => S]) => CookieService[T, S](ident, default, desc, s.map((_: E1 => S) compose convert))
  }


  def field[S, F1 <: JValue](s1AndDefault: IdentifierWithDefault[Symbol, F1])(implicit mc1: Manifest[F1]): HttpService[JValue, F1 => S] => HttpService[JValue, S] = {
    def extractField[F <: JValue](content: JValue, s1AndDefault: IdentifierWithDefault[Symbol, F])(implicit mc: Manifest[F]): F = {
      val c: Class[F] = mc.erasure.asInstanceOf[Class[F]]

      ((content \ s1AndDefault.identifier.name) -->? c).getOrElse(s1AndDefault.default).asInstanceOf[F]
    }

    extract[JValue, S, F1]{ (request: HttpRequest[JValue]) =>
      val content = request.content.getOrElse(sys.error("Expected request body to be JSON object"))

      extractField(content, s1AndDefault)
    }
  }

  /** The accept combinator creates a handler that is defined only for requests
   * that have the specified content type. Requires an implicit bijection
   * used for transcoding.
   */
  def accept[T, S, U](mimeType: MimeType)(h: HttpService[Future[T], Future[HttpResponse[S]]])(implicit b: Bijection[U, Future[T]]): HttpService[U, Future[HttpResponse[S]]] = new AcceptService[T, S, U](mimeType, h)

  /** The accept combinator creates a handler that is defined only for requests
   * that have the specified content type. Requires an implicit bijection
   * used for transcoding.
   */
  def accept2[T, S, U, E1](mimeType: MimeType)(h: HttpService[Future[T], E1 => Future[HttpResponse[S]]])(implicit b: Bijection[U, Future[T]]): HttpService[U, E1 => Future[HttpResponse[S]]] = new Accept2Service[T, S, U, E1](mimeType, h)

  /** The produce combinator creates a handler that is produces responses
   * that have the specified content type. Requires an implicit bijection
   * used for transcoding.
   */
  def produce[T, S, V](mimeType: MimeType)(h: HttpService[T, Future[HttpResponse[S]]])(implicit b: Bijection[S, V]): HttpService[T, Future[HttpResponse[V]]] = new ProduceService(mimeType, h)

  /** The produce combinator creates a handler that is produces responses
   * that have the specified content type. Requires an implicit bijection
   * used for transcoding.
   */
  def produce2[T, S, V, E1](mimeType: MimeType)(h: HttpService[T, E1 => Future[HttpResponse[S]]])(implicit b: Bijection[S, V]): HttpService[T, E1 => Future[HttpResponse[V]]] = new Produce2Service(mimeType, h)
  /** The content type combinator creates a handler that accepts and produces
   * requests and responses of the specified content type. Requires an implicit
   * bijection used for transcoding.
   */
  def contentType[T, S](mimeType: MimeType)(h: HttpService[Future[T], Future[HttpResponse[T]]])(implicit b1: Bijection[S, Future[T]], b2: Bijection[T, S]): HttpService[S, Future[HttpResponse[S]]] = {
    implicit val b3 = b2.inverse

    accept(mimeType) {
      produce[Future[T], T, S](mimeType) {
        h
      }
    }
  }
  def contentType2[T, S, E1](mimeType: MimeType)(h: HttpService[Future[T], E1 => Future[HttpResponse[T]]])(implicit b1: Bijection[S, Future[T]], b2: Bijection[T, S]): HttpService[S, E1 => Future[HttpResponse[S]]] = {
    implicit val b3 = b2.inverse

    accept2(mimeType) {
      produce2[Future[T], T, S, E1](mimeType) {
        h
      }
    }
  }

  /**
   *  The compress combinator creates a handler that compresses content by encoding supported by client
   *  (specified by Accept-Encoding header). The combinator supports gzip and deflate encoding.
   */
  def compress(h: HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]])(implicit supportedCompressions: Map[Encoding, CompressedByteChunk] = CompressService.supportedCompressions): HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]] = new CompressService(h)

  def compress2[E1](h: HttpService[ByteChunk, E1 => Future[HttpResponse[ByteChunk]]])(implicit supportedCompressions: Map[Encoding, CompressedByteChunk] = CompressService.supportedCompressions): HttpService[ByteChunk, E1 => Future[HttpResponse[ByteChunk]]] = new CompressService2[E1](h)

  /** The aggregate combinator creates a handler that stitches together chunks
   * to make a bigger chunk, up to the specified size.
   */
  def aggregate(chunkSize: Option[DataSize])(h: HttpService[Future[ByteChunk], Future[HttpResponse[ByteChunk]]]): HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]] = new AggregateService(chunkSize, h)

  def aggregate2[E1](chunkSize: Option[DataSize])(h: HttpService[Future[ByteChunk], E1 => Future[HttpResponse[ByteChunk]]]): HttpService[ByteChunk, E1 => Future[HttpResponse[ByteChunk]]] = new Aggregate2Service[E1](chunkSize, h)

  /** The jsonp combinator creates a handler that accepts and produces JSON.
   * The handler also transparently works with JSONP, if the client specifies
   * a "callback" parameter in the query string. Clients may encode both
   * HTTP method and content using the query string parameters "method" and
   * "content", respectively.
   */
  def jsonp[T](delegate: HttpService[JValue, Future[HttpResponse[JValue]]])(implicit b1: Bijection[T, JValue], bstr: Bijection[T, String]): HttpService[T, Future[HttpResponse[T]]] = JsonpService[T](delegate)

  def jsonp2[T, E1](delegate: HttpService[JValue, E1 => Future[HttpResponse[JValue]]])(implicit b1: Bijection[T, JValue], bstr: Bijection[T, String]): HttpService[T, E1 => Future[HttpResponse[T]]] = Jsonp2Service[T, E1](delegate)

  /** The jvalue combinator creates a handler that accepts and produces JSON.
   * Requires an implicit bijection used for transcoding.
   */
  def jvalue[T](h: HttpService[Future[JValue], Future[HttpResponse[JValue]]])(implicit b1: Bijection[T, Future[JValue]], b2: Bijection[JValue, T]): HttpService[T, Future[HttpResponse[T]]] = contentType(MimeTypes.application/MimeTypes.json) { h }

  def jvalue2[T, E1](h: HttpService[Future[JValue], E1 => Future[HttpResponse[JValue]]])(implicit b1: Bijection[T, Future[JValue]], b2: Bijection[JValue, T]): HttpService[T, E1 => Future[HttpResponse[T]]] = contentType2(MimeTypes.application/MimeTypes.json) { h }

  /** The xml combinator creates a handler that accepts and produces XML.
   * Requires an implicit bijection used for transcoding.
   */
  def xml[T](h: HttpService[Future[NodeSeq], Future[HttpResponse[NodeSeq]]])(implicit b1: Bijection[T, Future[NodeSeq]], b2: Bijection[NodeSeq, T]): HttpService[T, Future[HttpResponse[T]]] = contentType(MimeTypes.text/MimeTypes.xml) { h }

  def xml2[T, E1](h: HttpService[Future[NodeSeq], E1 => Future[HttpResponse[NodeSeq]]])(implicit b1: Bijection[T, Future[NodeSeq]], b2: Bijection[NodeSeq, T]): HttpService[T, E1 => Future[HttpResponse[T]]] = contentType2(MimeTypes.text/MimeTypes.xml) { h }

  def forwarding[T, U](f: HttpRequest[T] => Option[HttpRequest[U]], httpClient: HttpClient[U]) = (h: HttpService[T, HttpResponse[T]]) => new ForwardingService[T, U](f, httpClient, h)

  def metadata[T, S](metadata: Metadata*)(h: HttpService[T, HttpResponse[S]]) = MetadataService(Some(AndMetadata(metadata: _*)), h)

  def describe[T, S](description: String)(h: HttpService[T, HttpResponse[S]]) = MetadataService(Some(DescriptionMetadata(description)), h)

  /** The decodeUrl combinator creates a handler that decode HttpRequest URI.
   */
  def decodeUrl[T, S](h: HttpService[T, S]) = new DecodeUrlService[T, S](h)
}


class IdentifierWithDefault[A, B](val identifier: A, dflt: => Option[B]) {
  lazy val default = dflt
}

object IdentifierWithDefault {
  def apply[A, B](a: A, dflt: => Option[B]) = new IdentifierWithDefault(a, dflt)
  def unapply[A, B](idwd: IdentifierWithDefault[A, B]) = Some((idwd.identifier, idwd.default))
}
