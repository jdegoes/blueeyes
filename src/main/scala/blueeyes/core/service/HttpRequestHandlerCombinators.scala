package blueeyes
package core.service

import blueeyes.concurrent.Future
import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.data.{ByteChunk, Bijection, AggregatedByteChunk, ZLIBByteChunk, GZIPByteChunk, CompressedByteChunk}
import blueeyes.json.JsonAST._
import blueeyes.util.metrics.DataSize

import java.io.ByteArrayOutputStream
import java.net.URLDecoder._

import scala.xml.NodeSeq
import scalaz.Scalaz._

trait HttpRequestHandlerCombinators{
  private val supportedCompressions = Map[Encoding, CompressedByteChunk](Encodings.gzip -> GZIPByteChunk, Encodings.deflate -> ZLIBByteChunk)

  /** The path combinator creates a handler that is defined only for suffixes
   * of the specified path pattern.
   *
   * {{{
   * path("/foo") {
   *   ...
   * }
   * }}}
   */
  def path[T, S](path: RestPathPattern) = (h: HttpRequestHandler2[T, S]) => new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = path.isDefinedAt(r.subpath) && h.isDefinedAt(path.shift(r))

    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = {
      val pathParameters = path(r.subpath).map(parameter => (parameter._1, decode(parameter._2, "UTF-8")))

      val shiftedRequest = path.shift(r)

      h(shiftedRequest.copy(parameters = shiftedRequest.parameters ++ pathParameters))
    }
  }

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
  def remainingPath[T, S](handler: String => HttpRequestHandler2[T, S]) = path(RestPathPattern.Root `...` ('remainingPath)) {
    parameter(IdentifierWithDefault('remainingPath, () => "")) {
      handler
    }
  }

  /** The method combinator creates a handler that is defined only for the
   * specified HTTP method.
   */
  def method[T, S](method: HttpMethod) = (h: HttpRequestHandler2[T, S]) => new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = r.method == method && h.isDefinedAt(r)

    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = r.method match {
      case `method` => h(r)

      case _ => sys.error("The handler " + h + " can only respond to HTTP method " + method)
    }
  }

  /**
   * <pre>
   * path("/foo") {
   *   ...
   * } ~ orFail { request => BadRequest -> "The path " + request.path + " is malformed" }
   * </pre>
   */
  def orFail[T, S](h: HttpRequest[T] => (HttpFailure, String)): HttpRequestHandler2[T, S] = new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = true

    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = {
      Future.dead(h(r).fold(HttpException(_, _)))
    }
  }

  /**
   * <pre>
   * path("/foo") {
   *   ...
   * } ~ orFail(HttpStatusCodes.NotFound, "No handler found that could handle this request.")
   * </pre>
   */
  def orFail[T, S](code: HttpFailure, msg: String): HttpRequestHandler2[T, S] = orFail { request => code -> msg }
  
  /**
   * <pre>
   * path("/foo") {
   *   ...
   * } ~ orFail("The path is malformed")
   * </pre>
   */
  def orFail[T, S](msg: String): HttpRequestHandler2[T, S] = orFail { request => HttpStatusCodes.BadRequest -> msg }

  /** The path end combinator creates a handler that is defined only for paths
   * that are fully matched.
   */
  def $ [T, S](h: HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = path(RestPathPatternParsers.EmptyPathPattern) { h }

  /** Forces a particular combinator to match.
   * <pre>
   * commit(r => BadRequest -> "Bad path: " + r.path) {
   *   path("/foo") {
   *     ...
   *   }
   * }
   * </pre>
   */
  def commit[T, S](msgGen: HttpRequest[T] => (HttpFailure, String))(h: HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = true

    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = {
      if (h.isDefinedAt(r)) h(r) else Future.dead(msgGen(r).fold(HttpException(_, _)))
    }
  }

  /** Converts a full request handler into a partial request handler that
   * handles every input. Note: This is an implicit and will automatically
   * convert all full request handlers into partial request handlers,
   * as required by type signatures.
   */
  implicit def commitFull[T, S](h: HttpRequest[T] => Future[HttpResponse[S]]): HttpRequestHandler2[T, S] = new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = true

    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = h(r)
  }

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
  def justTry[T, S](h: HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = {
      try {
        h.isDefinedAt(r)
      } catch {
        case _ => false
      }
    }

    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = h(r)
  }

  def get     [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.GET)      { commitFull { h } } }
  def put     [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.PUT)      { commitFull { h } } }
  def post    [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.POST)     { commitFull { h } } }
  def delete  [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.DELETE)   { commitFull { h } } }
  def head    [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.HEAD)     { commitFull { h } } }
  def patch   [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.PATCH)    { commitFull { h } } }
  def options [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.OPTIONS)  { commitFull { h } } }
  def trace   [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.TRACE)    { commitFull { h } } }
  def connect [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.CONNECT)  { commitFull { h } } }

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
  def getRange[T, S](h: (List[(Long, Long)], String) => HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = method(HttpMethods.GET) {
    new HttpRequestHandler2[T, S] {
      private def extractRange(headers: Map[String, String]): Option[(List[(Long, Long)], String)] = {
        headers.collect {
          case (name, value) if name.toLowerCase == "range" => 
            value.split("=").toList match {
              case unit :: specifiers :: Nil => Tuple2(
                specifiers.split(",").toList.map { 
                  _.split("-").toList.map(_.trim.toLong) match {
                    case lowerBound :: upperBound :: Nil => (lowerBound, upperBound)
                    case _ => sys.error("missing upper and/or lower bound for range")
                  }
                }, 
                unit.trim.toLowerCase
              )

              case _ => sys.error("missing range specifier")
            }
        }.headOption
      }

      def isDefinedAt(r: HttpRequest[T]): Boolean = {
        (extractRange(r.headers.raw)).map(h.tupled).exists(_.isDefinedAt(r))
      }

      def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = {
        extractRange(r.headers.raw).map(h.tupled).get.apply(r)
      }
    }
  }

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
  def extract[T, S, E1](extractor: HttpRequest[T] => E1) = (h: E1 => HttpRequestHandler2[T, S]) => new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = {
      try {
        val extracted = extractor(r)

        h(extracted).isDefinedAt(r)
      }
      catch {
        case t: Throwable => throw HttpException(HttpStatusCodes.BadRequest, t)
      }
    }

    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = {
      val extracted = extractor(r)

      h(extracted).apply(r)
    }
  }

  /** A special-case extractor for parameters.
   * <pre>
   * parameter('token) { token =>
   *   get {
   *     ...
   *   }
   * }
   * </pre>
   */
  def parameter[T, S](s1AndDefault: IdentifierWithDefault[Symbol, String]) = (h: String => HttpRequestHandler2[T, S]) =>  new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = {
      try {
        val value = extract(r)

        h(value).isDefinedAt(addParameter(r, (s1AndDefault.identifier -> value)))
      }
      catch {
        case t: Throwable => throw HttpException(HttpStatusCodes.BadRequest, t)
      }
    }

    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = {
      val value = extract(r)

      h(value).apply(addParameter(r, (s1AndDefault.identifier -> value)))
    }

    private def extract(r: HttpRequest[T]): String = {
      r.parameters.get(s1AndDefault.identifier).getOrElse(s1AndDefault.default)
    }

    private def addParameter(r: HttpRequest[T], newParam: (Symbol, String)): HttpRequest[T] = {
      r.copy(parameters = r.parameters + newParam)
    }
  }

  private def extractCookie[T](request: HttpRequest[T], s: Symbol, defaultValue: Option[String] = None) = {
    def cookies = (for (HttpHeaders.Cookie(value) <- request.headers.raw) yield HttpHeaders.Cookie(value)).headOption.getOrElse(HttpHeaders.Cookie(Nil))
    cookies.cookies.find(_.name == s.name).map(_.cookieValue).orElse(defaultValue).getOrElse(sys.error("Expected cookie " + s.name))
  }

  /** A special-case extractor for cookie.
   * <pre>
   * cookie('token) { token =>
   *   get {
   *     ...
   *   }
   * }
   * </pre>
   */
  def cookie[T, S](s1: Symbol)(h: String => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = extract[T, S, String] { request =>
    extractCookie(request, s1)
  } { h }

  /** A special-case extractor for cookie supporting a default value.
   * <pre>
   * cookie('token, "defaultValue") { token =>
   *   get {
   *     ...
   *   }
   * }
   * </pre>
   */
  def cookie[T, S](s1AndDefault: IdentifierWithDefault[Symbol, String])(h: String => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = extract[T, S, String] { request =>
    extractCookie(request, s1AndDefault.identifier, Some(s1AndDefault.default))
  } { h }

  private def extractField[F <: JValue](content: JValue, s1AndDefault: IdentifierWithDefault[Symbol, F])(implicit mc: Manifest[F]): F = {
    val c: Class[F] = mc.erasure.asInstanceOf[Class[F]]

    ((content \ s1AndDefault.identifier.name) -->? c).getOrElse(s1AndDefault.default).asInstanceOf[F]
  }

  private def fieldError[F <: JValue](s: Symbol, mc: Manifest[F])(): F  = sys.error("Expected field " + s.name + " to be " + mc.erasure.asInstanceOf[Class[F]].getName)

  def field[S, F1 <: JValue](s1AndDefault: IdentifierWithDefault[Symbol, F1])(implicit mc1: Manifest[F1]) = (h: F1 => HttpRequestHandler2[JValue, S]) => {
    val c1: Class[F1] = mc1.erasure.asInstanceOf[Class[F1]]

    extract[JValue, S, F1] { (request: HttpRequest[JValue]) =>
      val content = request.content.getOrElse(sys.error("Expected request body to be JSON object"))

      extractField(content, s1AndDefault)
    } (h)
  }

  /** The accept combinator creates a handler that is defined only for requests
   * that have the specified content type. Requires an implicit bijection
   * used for transcoding.
   */
  def accept[T, S, U](mimeType: MimeType)(h: HttpRequestHandler2[T, S])(implicit b: Bijection[U, T]): HttpRequestHandler2[U, S] = new HttpRequestHandler2[U, S] {
    def isDefinedAt(r: HttpRequest[U]): Boolean = {

      r.mimeTypes.find(_ == mimeType).map { mimeType =>
        h.isDefinedAt(r.copy(content = r.content.map(b.apply)))
      }.orElse {
        r.content.map(b.isDefinedAt _)
      }.getOrElse(false)
    }

    def apply(r: HttpRequest[U]) = h(r.copy(content = r.content.map(b.apply)))
  }

  /** The produce combinator creates a handler that is produces responses
   * that have the specified content type. Requires an implicit bijection
   * used for transcoding.
   */
  def produce[T, S, V](mimeType: MimeType)(h: HttpRequestHandler2[T, S])(implicit b: Bijection[S, V]): HttpRequestHandler2[T, V] = new HttpRequestHandler2[T, V] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = h.isDefinedAt(r)

    def apply(r: HttpRequest[T]): Future[HttpResponse[V]] = h(r).map { response =>
      response.copy(content = response.content.map(b.apply), headers = response.headers + `Content-Type`(mimeType))
    }
  }

  /** The content type combinator creates a handler that accepts and produces
   * requests and responses of the specified content type. Requires an implicit
   * bijection used for transcoding.
   */
  def contentType[T, S](mimeType: MimeType)(h: HttpRequestHandler[T])(implicit b1: Bijection[S, T]): HttpRequestHandler[S] = {
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
  def compress(h: HttpRequestHandler[ByteChunk]): HttpRequestHandler[ByteChunk] = new HttpRequestHandler[ByteChunk] {
    def isDefinedAt(r: HttpRequest[ByteChunk]) = h.isDefinedAt(r)

    def apply(r: HttpRequest[ByteChunk]) = {
      h(r).map{response =>
        val encodings  = r.headers.header(`Accept-Encoding`).map(_.encodings.toList).getOrElse(Nil)
        val supported  = supportedCompressions.filterKeys(encodings.contains(_)).headOption.map(_._2)
        (supported, response.content) match{
          case (Some(compression), Some(content)) => response.copy(content = Some(compression(content)))
          case _ => response
        }
      }
    }
  }

  /** The aggregate combinator creates a handler that stitches together chunks
   * to make a bigger chunk, up to the specified size.
   */
  def aggregate(chunkSize: Option[DataSize])(h: HttpRequestHandler[ByteChunk]): HttpRequestHandler[ByteChunk] = new HttpRequestHandler[ByteChunk] {
    val chunkSizeInBytes = chunkSize.map(_.bytes)
    def isDefinedAt(r: HttpRequest[ByteChunk]) = h.isDefinedAt(r)

    def apply(r: HttpRequest[ByteChunk]) = r.content match {
      case Some(chunk) =>
        val result = new Future[HttpResponse[ByteChunk]]()
        val aggregatedFuture = AggregatedByteChunk(chunk, chunkSize)
        aggregatedFuture.deliverTo{aggregated =>
          val f = h(r.copy(content = Some(aggregated)))

          f.deliverTo(response => result.deliver(response))
          f.ifCanceled(th => result.cancel(th))
          result.ifCanceled(th => f.cancel(th))
        }
        aggregatedFuture.ifCanceled(th => result.cancel(th))
        result.ifCanceled(th => aggregatedFuture.cancel(th))
        result
      case None        => h(r)
    }
  }

  /** The jsonp combinator creates a handler that accepts and produces JSON.
   * The handler also transparently works with JSONP, if the client specifies
   * a "callback" parameter in the query string. Clients may encode both
   * HTTP method and content using the query string parameters "method" and
   * "content", respectively.
   */
  def jsonp[T](delegateHandler: HttpRequestHandler[JValue])(implicit b1: Bijection[T, JValue], bstr: Bijection[T, String]): HttpRequestHandler[T] = new HttpRequestHandler[T] {
    implicit val b2 = b1.inverse

    def isDefinedAt(r: HttpRequest[T]): Boolean = {
      r.content.forall(b1.isDefinedAt _) && delegateHandler.isDefinedAt(convertRequest(r))
    }

    def apply(r: HttpRequest[T]): Future[HttpResponse[T]] = {
      delegateHandler(convertRequest(r)).map(convertResponse(_, r.parameters.get('callback)))
    }

    private def convertRequest(r: HttpRequest[T]): HttpRequest[JValue] = {
      import blueeyes.json.JsonParser.parse
      import blueeyes.json.xschema.DefaultSerialization._

      r.parameters.get('callback) match {
        case Some(callback) if (r.method == HttpMethods.GET) =>
          if (!r.content.isEmpty) throw HttpException(HttpStatusCodes.BadRequest, "JSONP requested but content body is non-empty")

          val methodStr = r.parameters.get('method).getOrElse("get").toUpperCase

          val method  = HttpMethods.PredefinedHttpMethods.find(_.value == methodStr).getOrElse(HttpMethods.GET)
          val content = r.parameters.get('content).map(parse _)
          val headers = r.parameters.get('headers).map(parse _).map(_.deserialize[Map[String, String]]).getOrElse(Map.empty[String, String])

          r.copy(method = method, content = content, headers = r.headers ++ headers)

        case Some(callback) =>
          throw HttpException(HttpStatusCodes.BadRequest, "JSONP requested but HTTP method is not GET")

        case None =>
          r.copy(content = r.content.map(b1.apply))
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
  }

  /** The jvalue combinator creates a handler that accepts and produces JSON.
   * Requires an implicit bijection used for transcoding.
   */
  def jvalue[T](h: HttpRequestHandler[JValue])(implicit b: Bijection[T, JValue]): HttpRequestHandler[T] = contentType(MimeTypes.application/MimeTypes.json) { h }

  /** The xml combinator creates a handler that accepts and produces XML.
   * Requires an implicit bijection used for transcoding.
   */
  def xml[T](h: HttpRequestHandler[NodeSeq])(implicit b: Bijection[T, NodeSeq]): HttpRequestHandler[T] = contentType(MimeTypes.text/MimeTypes.xml) { h }

  def forwarding[T](f: HttpRequest[T] => Option[HttpRequest[T]])(implicit httpClient: HttpClient[T]) = (h: HttpRequestHandler[T]) => new HttpRequestHandler[T] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = h.isDefinedAt(r)

    def apply(r: HttpRequest[T]): Future[HttpResponse[T]] = {
      Future.async(f(r).foreach(httpClient))
      h(r)
    }
  }

  /** The decodeUrl combinator creates a handler that decode HttpRequest URI.
   */
  def decodeUrl[T, S](h: HttpRequestHandler2[T, S]) = new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = h.isDefinedAt(r)

    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = h(r.copy(uri = r.uri.decode))
  }
}


case class IdentifierWithDefault[T, S](identifier: T, default_ : () => S) {
  def default = default_ ()
}
