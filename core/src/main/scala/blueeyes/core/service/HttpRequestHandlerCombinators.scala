package blueeyes.core
package service

import http._
import http.HttpHeaders._
import http.HttpStatusCodes._
import data._

import akka.dispatch.Future
import akka.dispatch.ExecutionContext
import blueeyes.json._
import blueeyes.util.metrics.DataSize

import com.weiglewilczek.slf4s.Logger

import scala.xml.NodeSeq
import scalaz.Validation
import scalaz.Semigroup
import scalaz.Monad


trait HttpRequestHandlerCombinators {
  implicit def handle[A, B](handler: HttpServiceHandler[A, B]): HttpService[A, B] = new HttpHandlerService(handler)

  def debug[A, B](logger: Logger): HttpService[A, B] => HttpService[A, B] =
    (h: HttpService[A, B]) => new DebugService[A, B](logger, h)

  /** The path combinator creates a handler that is defined only for suffixes
   * of the specified path pattern.
   *
   * {{{
   * path("/foo") {
   *   ...
   * }
   * }}}
   */
  def path[A, B](path: RestPathPattern): HttpService[A, B] => HttpService[A, B] =
    (h: HttpService[A, B]) => new PathService[A, B] (path, h)

  def pathParameter[A, B](path: RestPathPattern, sym: Symbol) =
    (h: HttpService[A, String => B]) => new PathParameterService(path, sym, h)

  def pathData[A, B, C](path: RestPathPattern, sym: Symbol, f: String => Validation[NotServed, B]) =
    (h: HttpService[A, B => C]) => new PathDataService(path, sym, f, h)

  /** The path end combinator creates a handler that is defined only for paths
   * that are fully matched.
   */
  def $ [A, B] = path[A, B](RestPathPatternParsers.EmptyPathPattern)

  /** Yields the remaining path to the specified function, which should return
   * a request handler.
   * {{{
   * remainingPath {
   *   get {  (request: HttpRequest[A]) => { path =>
   *     ...
   *   }}
   * }
   * }}}
   */
  def remainingPath[A, B] = path(RestPathPattern.Root `...` ('remainingPath)) andThen parameter[A, B]('remainingPath, Some(""))

  /** The method combinator creates a handler that is defined only for the
   * specified HTTP method.
   */
  def method[A, B](method: HttpMethod): HttpService[A, B] => HttpService[A, B] = (h: HttpService[A, B]) => new HttpMethodService[A, B] (method, h)

  /**
   * <pre>
   * path("/foo") {
   *   ...
   * } ~ orFail { request => BadRequest -> "The path " + request.path + " is malformed" }
   * </pre>
   */
  def orFail[A, B](h: HttpRequest[A] => (HttpFailure, String)): HttpService[A, B] = new FailureService[A, B](h)

  /**
   * <pre>
   * path("/foo") {
   *   ...
   * } ~ orFail(HttpStatusCodes.NotFound, "No handler found that could handle this request.")
   * </pre>
   */
  def orFail[A, B](code: HttpFailure, msg: String): HttpService[A, B] = orFail { request => code -> msg }

  /**
   * <pre>
   * path("/foo") {
   *   ...
   * } ~ orFail("The path is malformed")
   * </pre>
   */
  def orFail[A, B](msg: String): HttpService[A, B] = orFail(BadRequest, msg)

  /** Forces a particular combinator to match.
   * <pre>
   * commit(r => BadRequest -> "Bad path: " + r.path) {
   *   path("/foo") {
   *     ...
   *   }
   * }
   * </pre>
   */
  def commit[A, B](delegate: HttpService[A, B]): HttpService[A, B] = new CommitService(delegate)

  def get     [A, B](h: HttpService[A, B]): HttpService[A, B] = $ { method(HttpMethods.GET)     { h } }
  def put     [A, B](h: HttpService[A, B]): HttpService[A, B] = $ { method(HttpMethods.PUT)     { h } }
  def post    [A, B](h: HttpService[A, B]): HttpService[A, B] = $ { method(HttpMethods.POST)    { h } }
  def delete  [A, B](h: HttpService[A, B]): HttpService[A, B] = $ { method(HttpMethods.DELETE)  { h } }
  def head    [A, B](h: HttpService[A, B]): HttpService[A, B] = $ { method(HttpMethods.HEAD)    { h } }
  def patch   [A, B](h: HttpService[A, B]): HttpService[A, B] = $ { method(HttpMethods.PATCH)   { h } }
  def options [A, B](h: HttpService[A, B]): HttpService[A, B] = $ { method(HttpMethods.OPTIONS) { h } }
  def trace   [A, B](h: HttpService[A, B]): HttpService[A, B] = $ { method(HttpMethods.TRACE)   { h } }
  def connect [A, B](h: HttpService[A, B]): HttpService[A, B] = $ { method(HttpMethods.CONNECT) { h } }

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
  def getRange[A, B](delegate: HttpService[A, RangeHeaderValues => B]): HttpService[A, B] = {
    val s0 = new GetRangeService(delegate) 
    method(HttpMethods.GET)(s0)
  }

  /**
   * Extracts data from the request. The extractor combinators can be used to
   * factor out extraction logic that's duplicated across a range of handlers.
   * <p>
   * Extractors are fail-fast combinators. If they cannot extract the required
   * information during evaluation of isDefinedAt() method, they immediately
   * throw an HttpException.
   * <pre>
   * extract(_.parameters('username)) {
   *   get {  (request: HttpRequest[A]) => {username =>
   *     ...
   *   }}
   * }
   * </pre>
   */
  def extract[A, B, E1](extractor: HttpRequest[A] => E1): HttpService[A, E1 => B] => HttpService[A, B] = 
    new ExtractService[A, B, E1](extractor, _)

  /** A special-case extractor for parameters.
   * <pre>
   * parameter('token) {
   *   get {  (request: HttpRequest[A]) => {token =>
   *     ...
   *   }}
   * }
   * </pre>
   */
  def parameter[A, B](parameter: Symbol, default: => Option[String] = None) = {
    new ParameterService[A, B](parameter, default, _: HttpService[A, String => B])
  }

  def convertedParameter[A, B, E1](parameter: Symbol, convert: String => E1, default: => Option[String] = None) = {
    (s: HttpService[A, E1 => B]) => ParameterService[A, B](parameter, default, s.map((_: E1 => B) compose convert))
  }

  /** A special-case extractor for cookie supporting a default value.
   * <pre>
   * cookie('token, "defaultValue") {
   *   get {  (request: HttpRequest[A]) => { token =>
   *     ...
   *   }}
   * }
   * </pre>
   */
  def cookie[A, B](ident: Symbol, default: => Option[String] = None) = {
    new CookieService[A, B](ident, default, _: HttpService[A, String => B])
  }

  def convertedCookie[A, B, E1](ident: Symbol, convert: String => E1, default: => Option[String] = None) = {
    (s: HttpService[A, E1 => B]) => CookieService[A, B](ident, default, s.map((_: E1 => B) compose convert))
  }

  def field[B, F1 <: JValue](s1AndDefault: IdentifierWithDefault[Symbol, F1])(implicit mc1: Manifest[F1]): HttpService[JValue, F1 => B] => HttpService[JValue, B] = {
    def extractField[F <: JValue](content: JValue, s1AndDefault: IdentifierWithDefault[Symbol, F])(implicit mc: Manifest[F]): F = {
      val c: Class[F] = mc.erasure.asInstanceOf[Class[F]]

      ((content \ s1AndDefault.identifier.name) -->? c).getOrElse(s1AndDefault.default).asInstanceOf[F]
    }

    extract[JValue, B, F1]{ (request: HttpRequest[JValue]) =>
      val content = request.content.getOrElse(sys.error("Expected request body to be JSON object"))

      extractField(content, s1AndDefault)
    }
  }

  implicit def transcode[A, B](h: HttpService[Future[B], Future[HttpResponse[B]]])(implicit inj: A => Future[B], surj: B => A) = 
    new TranscodeService[A, B](h)

  /** The accept combinator creates a handler that is defined only for requests
   * that have the specified content type. Requires an implicit function
   * used for transcoding.
   */
  def accept[A, B](mimeTypes: MimeType*)(h: HttpService[A, B]): HttpService[A, B] = 
    new AcceptService(mimeTypes, h)

  /** The produce combinator creates a handler that is produces responses
   * that have the specified content type. Requires an implicit function
   * used for transcoding.
   */
  def produce[A, B](mimeType: MimeType)(h: HttpService[A, B])(implicit modifier: ResponseModifier[B]): HttpService[A, B] = 
    new ProduceService(mimeType, h, modifier)

  def decode[A, B, C](h: HttpService[B, C])(implicit f: A => B): HttpService[A, C] = 
    h.contramap(f)

  def encode[A, B, C](h: HttpService[A, B])(implicit f: B => C): HttpService[A, C] = 
    h.map(f)

  /** The content type combinator creates a handler that accepts and produces
   * requests and responses of the specified content type. Requires an implicit
   * bijection used for transcoding.
   */
  def contentType[A, B](mimeType: MimeType)(h: HttpService[A, B])(implicit modifier: ResponseModifier[B]): HttpService[A, B] = 
    accept(mimeType) {
      produce(mimeType) { h }
    }

  /** The aggregate combinator creates a handler that stitches together chunks
   * to make a bigger chunk, up to the specified size.
   */
  def aggregate(chunkSize: Option[DataSize])(h: HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]])(implicit executor: ExecutionContext) = 
    new AggregateService(chunkSize, h)

  /** The jsonp combinator creates a handler that accepts and produces JSON.
   * The handler also transparently works with JSONP, if the client specifies
   * a "callback" parameter in the query string. Clients may encode both
   * HTTP method and content using the query string parameters "method" and
   * "content", respectively.
   */
  def jsonp[A](delegate: HttpService[A, Future[HttpResponse[A]]])(implicit fromString: String => A, semigroup: Semigroup[A]) = 
    new JsonpService[A](delegate)

  /** The jvalue combinator creates a handler that accepts and produces JSON.
   * Requires an implicit bijection used for transcoding.
   */
  def jvalue[A](h: HttpService[Future[JValue], Future[HttpResponse[JValue]]])(implicit inj: A => Future[JValue], surj: JValue => A, M: Monad[Future]): HttpService[A, Future[HttpResponse[A]]] = 
    contentType(MimeTypes.application/MimeTypes.json) { h.contramap(inj) } map { _ map { _ map surj } }

  /** The xml combinator creates a handler that accepts and produces XML.
   * Requires an implicit bijection used for transcoding.
   */
  def xml[A](h: HttpService[Future[NodeSeq], Future[HttpResponse[NodeSeq]]])(implicit inj: A => Future[NodeSeq], surj: NodeSeq => A, M: Monad[Future]) = 
    contentType(MimeTypes.text/MimeTypes.xml) { h.contramap(inj) } map { _ map { _ map surj } }

  def forwarding[A, A0](f: HttpRequest[A] => Option[HttpRequest[A0]], httpClient: HttpClient[A0])(h: HttpService[A, HttpResponse[A]]) = 
    new ForwardingService[A, A0](f, httpClient, h)

  def metadata[A, B](metadata: Metadata*)(h: HttpService[A, Future[HttpResponse[B]]]) = new MetadataService(Some(AndMetadata(metadata: _*)), h)

  def describe[A, B](description: String)(h: HttpService[A, Future[HttpResponse[B]]]) = new MetadataService(Some(DescriptionMetadata(description)), h)

  /** The decodeUrl combinator creates a handler that decode HttpRequest URI.
   */
  def decodeUrl[A, B](h: HttpService[A, B]) = new DecodeUrlService[A, B](h)
}

object HttpRequestHandlerCombinators extends HttpRequestHandlerCombinators

class IdentifierWithDefault[A, B](val identifier: A, dflt: => Option[B]) {
  lazy val default = dflt
}

object IdentifierWithDefault {
  def apply[A, B](a: A, dflt: => Option[B]) = new IdentifierWithDefault(a, dflt)
  def unapply[A, B](idwd: IdentifierWithDefault[A, B]) = Some((idwd.identifier, idwd.default))
}
