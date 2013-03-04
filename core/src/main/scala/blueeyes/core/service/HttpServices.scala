package blueeyes.core.service

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext

import blueeyes.bkka._
import blueeyes.core.data._
import blueeyes.core.data.Chunk._
import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders.{`Content-Type`, `Accept-Encoding`}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.json._
import blueeyes.json.serialization.DefaultSerialization._
import blueeyes.util.metrics.DataSize
import blueeyes.util.printer._

import java.net.URLDecoder._

import scalaz.{Validation, Success, Failure, Monad, StreamT, Semigroup}
import scalaz.syntax.pointed._
import scalaz.syntax.semigroup._
import scalaz.syntax.validation._
import scalaz.syntax.std.option._

import com.weiglewilczek.slf4s.Logger

sealed trait AnyService {
  def metadata: Option[Metadata]
  lazy val inapplicable: Inapplicable = Inapplicable(this)
}

sealed trait HttpService[A, B] extends AnyService { self =>
  def service: HttpRequest[A] => Validation[NotServed, B]

  def map[C](f: B => C): HttpService[A, C] = new HttpService[A, C] {
    override val service = (r: HttpRequest[A]) => self.service(r).map(f)
    override val metadata = self.metadata
  }

  def contramap[C](f: C => A): HttpService[C, B] = new HttpService[C, B] {
    override val service = (r: HttpRequest[C]) => self.service(r.copy(content = r.content.map(f)))
    override val metadata = self.metadata
  }

  def ~ (other: HttpService[A, B]): OrService[A, B] = OrService(self, other)
  def ~ [C, D](other: HttpService[C, D])(implicit unapply: Unapply[C, A], apply: D => B): OrService[A, B] = {
    self ~ other.contramap(unapply.unapply).map(apply)
  }

  def withMetadata(m: Metadata) = new MetadataService(Some(m), this)
}

/**
 * An open base trait for HTTP services that allows extension beyond
 * the sealed hierarchy.
 */
trait CustomHttpService[A, B] extends HttpService[A, B] 

trait DelegatingService[A, B, A0, B0] extends HttpService[A, B] {
  val delegate: HttpService[A0, B0]
}

case class OrService[A, B](services: HttpService[A, B]*) extends HttpService[A, B] {
  private def pick(r: HttpRequest[A], services: Seq[HttpService[A, B]]): Validation[NotServed, B] = {
    services.headOption.toSuccess(inapplicable) flatMap {
      _.service(r) match {
        case Failure(notServed) => notServed or pick(r, services.tail)
        case success => success
      }
    }
  }
 
  val service = (r: HttpRequest[A]) => pick(r, services)

  override def ~ (other: HttpService[A, B]) = other match {
    case OrService(other @ _*) => OrService(services ++ other: _*)
    case other => OrService(services :+ other: _*)
  }

  lazy val metadata = None
}

object DelegatingService {
  def unapply[A, B, C, D](s: DelegatingService[A, B, C, D]): Option[HttpService[C, D]] = Some(s.delegate)
}

class HttpHandlerService[A, B, C](h: HttpServiceHandler[B, C], f: A => B) extends CustomHttpService[A, C] {
  val service = (r: HttpRequest[A]) => h(r.map(f)).success

  val metadata = None
}

class FailureService[A, B](onFailure: HttpRequest[A] => (HttpFailure, String)) extends CustomHttpService[A, B] {
  val service = (r: HttpRequest[A]) => {
    val (errorCode, message) = onFailure(r)
    DispatchError(errorCode, message + " [" + r + "]").failure[B]
  }

  val metadata = None
}

class DebugService[A, B](logger: Logger, val delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
  val service = (request: HttpRequest[A]) => {
    println("Received request: " + request)
    logger.debug("Received request: " + request)
    delegate.service(request)
  }

  lazy val metadata = None
}


class PathService[A, B](path: RestPathPattern, val delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
  val service = PathService.shift(path, _: HttpRequest[A]).toSuccess(inapplicable).flatMap(delegate.service)

  lazy val metadata = Some(PathPatternMetadata(path))
}

object PathService {
  def shift[A](path: RestPathPattern, req: HttpRequest[A]): Option[HttpRequest[A]] = {
    if (path.isDefinedAt(req.subpath)) {
      val pathParameters = path(req.subpath).map(parameter => (parameter._1, decode(parameter._2, "UTF-8")))

      Some(path.shift(req).copy(parameters = req.parameters ++ pathParameters))
    } else {
      None
    }
  }
}

class HttpMethodService[A, B](method: HttpMethod, val delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
  val service = (r: HttpRequest[A]) => if (r.method == method) delegate.service(r) else inapplicable.failure[B]

  lazy val metadata = Some(HttpMethodMetadata(method))
}

class CommitService[A, B](val delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
  def pathMetadata(service: AnyService, services: Set[AnyService], path: Vector[Metadata]): Vector[Vector[Metadata]] = {
    if (services.contains(service)) Vector(path)
    else service match {
      case OrService(orMembers @ _*) => orMembers.flatMap((s: AnyService) => pathMetadata(s, services, path ++ s.metadata))(collection.breakOut)
      case DelegatingService(child)  => pathMetadata(child, services, path ++ child.metadata)
      case _ => Vector.empty[Vector[Metadata]]
    }
  }

  val service = (r: HttpRequest[A]) => delegate.service(r) match {
    case Failure(Inapplicable(services @ _*)) => 
      val paths = pathMetadata(delegate, services.toSet, Vector.empty[Metadata]).sortBy(-_.size)
      val longest = paths zip (paths.tail) takeWhile { case (a, b) => a.size == b.size } map (_._1)
      val metadata: Vector[Metadata] = (if (longest.isEmpty) paths.take(1) else longest) flatMap {
        _.foldLeft[Option[Metadata]](None)((a, m) => a.map(_ |+| m).orElse(Some(m)))
      }

      val message = "No service was found to be able to handle your request (" + r + "). \n" +
                    "Were you trying to access (one of) the following? \n" + 
                    metadata.map(SimpleStringPrinter.printFormatted[Metadata]).mkString("\n")
      
      DispatchError(NotFound, message).failure    
    case other => other
  }

  val metadata = None
}

class TranscodeService[A, B](val delegate: HttpService[Future[B], Future[HttpResponse[B]]])(implicit inj: A => Future[B], surj: B => A) 
extends DelegatingService[A, Future[HttpResponse[A]], Future[B], Future[HttpResponse[B]]] {
  val service = (request: HttpRequest[A]) => delegate.service(request.map(inj)).map(_.map(_.map(surj)))
  val metadata = None
}

class AcceptService[T, S, U](mimeType: MimeType, val delegate: HttpService[Future[T], Future[HttpResponse[S]]])(implicit f: U => Future[T]) 
extends DelegatingService[U, Future[HttpResponse[S]], Future[T], Future[HttpResponse[S]]] {
  import AcceptService._
  val service = (r: HttpRequest[U]) => convert(mimeType, r, inapplicable) flatMap { newRequest: HttpRequest[Future[T]] => 
    delegate.service(newRequest) map { checkConvert(newRequest, _) } 
  }

  lazy val metadata = Some(RequestHeaderMetadata(Right(`Content-Type`(mimeType))))
}

class Accept2Service[T, S, U, E1](mimeType: MimeType, val delegate: HttpService[Future[T], E1 => Future[HttpResponse[S]]])(implicit f: U => Future[T]) 
extends DelegatingService[U, E1 => Future[HttpResponse[S]], Future[T], E1 => Future[HttpResponse[S]]] {
  import AcceptService._
  val service = (r: HttpRequest[U]) => convert(mimeType, r, inapplicable) flatMap { newRequest: HttpRequest[Future[T]] =>
    delegate.service(newRequest).map(function => (e: E1) => function.apply(e))
  }

  lazy val metadata = Some(RequestHeaderMetadata(Right(`Content-Type`(mimeType))))
}

object AcceptService extends blueeyes.bkka.AkkaDefaults {
  def convert[U, T](mimeType: MimeType, r: HttpRequest[U], inapplicable: => Inapplicable)(implicit f: U => Future[T]) = {
    r.mimeTypes.find(_ == mimeType).map(_ => r.copy(content = r.content.map(f)).success).getOrElse(inapplicable.failure)
  }

  def checkConvert[T, S](request: HttpRequest[Future[T]], response: Future[HttpResponse[S]]) = {
    request.content.map{content =>
      val result = Promise[HttpResponse[S]]()
      content  onFailure { case error => result.success(HttpResponse[S](status = HttpStatus(BadRequest, error.getMessage))) }
      response onFailure { case error => result.failure(error) }
      response onSuccess { case value => result.success(value) }
      result
    }.getOrElse(response)
  }
}

class ProduceService[T, S, V](mimeType: MimeType, val delegate: HttpService[T, Future[HttpResponse[S]]], transcoder: S => V) 
extends DelegatingService[T, Future[HttpResponse[V]], T, Future[HttpResponse[S]]] {
  def service = (r: HttpRequest[T]) => delegate.service(r).map { 
    _.map(r => r.copy(content = r.content.map(transcoder), headers = r.headers + `Content-Type`(mimeType)))
  }

  lazy val metadata = Some(ResponseHeaderMetadata(Right(`Content-Type`(mimeType))))
}

class Produce2Service[T, S, V, E1](mimeType: MimeType, val delegate: HttpService[T, E1 => Future[HttpResponse[S]]], transcoder: S => V) 
extends DelegatingService[T, E1 => Future[HttpResponse[V]], T, E1 => Future[HttpResponse[S]]] {
  def service = (r: HttpRequest[T]) => delegate.service(r).map {
    f => f andThen ((_: Future[HttpResponse[S]]).map(r => r.copy(content = r.content.map(transcoder), headers = r.headers + `Content-Type`(mimeType))))
  }

  lazy val metadata = Some(ResponseHeaderMetadata(Right(`Content-Type`(mimeType))))
}

case class RangeHeaderValues(units: String, bounds: List[(Long, Long)])

class GetRangeService[T, S](val delegate: HttpService[T, RangeHeaderValues => S]) 
extends DelegatingService[T, S, T, RangeHeaderValues => S] {
  val service = (req: HttpRequest[T]) => extractRange(req.headers.raw) flatMap { t => 
    delegate.service(req).map(_.apply(t))
  }

  private def extractRange(headers: Map[String, String]): Validation[NotServed, RangeHeaderValues] = {
    val matchingHeaders = headers.collect {
      case (name, value) if name.toLowerCase == "range" =>
        value.split("=").toList match {
          case unit :: specifiers :: Nil =>
            val ranges = specifiers.split(",").toList.map {
              range => range.split("-").toList.map(_.trim.toLong) match {
                case lowerBound :: upperBound :: Nil => Right((lowerBound, upperBound))
                case _ => Left(range)
              }
            }

            val (badValues, goodValues) = ranges.foldLeft((List.empty[String], List.empty[(Long, Long)])) {
              case ((b, g), Left(badValue)) => (badValue :: b, g)
              case ((b, g), Right(goodValue)) => (b, goodValue :: g)
            }

            if (badValues.isEmpty) {
              RangeHeaderValues(unit.trim.toLowerCase, goodValues).success
            } else {
              DispatchError(BadRequest, "The following range header values were incorrectly formatted: " + badValues.mkString(",")).failure
            }

          case _ => DispatchError(BadRequest, "The range header value was incorrectly formatted: " + value).failure
        }
    }
    
    if (matchingHeaders.size > 1) {
      DispatchError(BadRequest, "Multiple range header fields specified; please only specify one header of each type.").failure
    } else {
      matchingHeaders.headOption.getOrElse(inapplicable.failure)
    }
  }

  lazy val metadata = Some(AndMetadata(RequestHeaderMetadata(Left(HttpHeaders.Range), None), DescriptionMetadata("A numeric range must be specified for the request.")))
}

class CompressService(val delegate: HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]])(implicit supportedCompressions: Map[Encoding, ChunkCompression]) 
extends DelegatingService[ByteChunk, Future[HttpResponse[ByteChunk]], ByteChunk, Future[HttpResponse[ByteChunk]]]{
  import CompressService._
  def service = (r: HttpRequest[ByteChunk]) => delegate.service(r).map{compress(r, _)}

  val metadata = Some(EncodingMetadata(supportedCompressions.keys.toSeq: _*))
}

class CompressService2[E1](val delegate: HttpService[ByteChunk, E1 => Future[HttpResponse[ByteChunk]]])(implicit supportedCompressions: Map[Encoding, ChunkCompression]) 
extends DelegatingService[ByteChunk, E1 => Future[HttpResponse[ByteChunk]], ByteChunk, E1 => Future[HttpResponse[ByteChunk]]]{

  import CompressService._
  def service = (r: HttpRequest[ByteChunk]) => delegate.service(r).map{function => (e: E1) => compress(r, function.apply(e))}

  val metadata = Some(EncodingMetadata(supportedCompressions.keys.toSeq: _*))
}

object CompressService {
  def compress(r: HttpRequest[ByteChunk], f: Future[HttpResponse[ByteChunk]])(implicit supportedCompressions: Map[Encoding, ChunkCompression]) = {
    for (response <- f) yield {
      val encodings = r.headers.header(`Accept-Encoding`).map(_.encodings.toList).getOrElse(Nil)
      val supported = supportedCompressions find { case (k, _) => encodings.contains(k) } map { _._2 }
      (supported, response.content) match {
        case (Some(compression), Some(content)) => response.copy(content = Some(compression.compress(content)))
        case _ => response
      }
    }
  }

  def defaultCompressions(implicit ctx: ExecutionContext) = Map[Encoding, ChunkCompression](
    Encodings.gzip -> ChunkCompression.gzip,
    Encodings.deflate -> ChunkCompression.zlib()
  )
}

class AggregateService(chunkSize: Option[DataSize], val delegate: HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]])(implicit executor: ExecutionContext) 
extends DelegatingService[ByteChunk, Future[HttpResponse[ByteChunk]], ByteChunk, Future[HttpResponse[ByteChunk]]]{
  private val size = chunkSize.map(_.intBytes).getOrElse(ByteChunk.defaultChunkSize)

  def service = (r: HttpRequest[ByteChunk]) => {
    delegate.service(r.copy(content = r.content.map(ByteChunk.aggregate(_, size))))
  }

  lazy val metadata = chunkSize.map(DataSizeMetadata)
}

class Aggregate2Service[E1](chunkSize: Option[DataSize], val delegate: HttpService[ByteChunk, E1 => Future[HttpResponse[ByteChunk]]])(implicit executor: ExecutionContext) 
extends DelegatingService[ByteChunk, E1 => Future[HttpResponse[ByteChunk]], ByteChunk, E1 => Future[HttpResponse[ByteChunk]]]{
  private val size = chunkSize.map(_.intBytes).getOrElse(ByteChunk.defaultChunkSize)

  def service = (r: HttpRequest[ByteChunk]) => {
    delegate.service(r.copy(content = r.content.map(ByteChunk.aggregate(_, size)))).map(f => (e: E1) => f(e))
  }

  lazy val metadata = chunkSize.map(DataSizeMetadata)
}

class JsonpService[T](val delegate: HttpService[T, Future[HttpResponse[T]]])(implicit fromString: String => T, semigroup: Semigroup[T]) 
extends DelegatingService[T, Future[HttpResponse[T]], T, Future[HttpResponse[T]]]{
  import JsonpService._
  def service = (r: HttpRequest[T]) => jsonpConvertRequest(r).flatMap(delegate.service).map(_.map(jsonpConvertResponse(_, r.parameters.get('callback))))

  val metadata = Some(OrMetadata(
    AndMetadata(
      HttpMethodMetadata(HttpMethods.GET),
      DescriptionMetadata("A callback method identifier is required when using JsonP with a \"GET\" request."),
      ParameterMetadata('callback, None)
    ),
    HttpMethodMetadata(HttpMethods.POST),
    HttpMethodMetadata(HttpMethods.PUT),
    HttpMethodMetadata(HttpMethods.DELETE)
  ))
}

class Jsonp2Service[T, E1](val delegate: HttpService[T, E1 => Future[HttpResponse[T]]])(implicit fromString: String => T, semigroup: Semigroup[T]) 
extends DelegatingService[T, E1 => Future[HttpResponse[T]], T, E1 => Future[HttpResponse[T]]]{
  import JsonpService._
  def service = (r: HttpRequest[T]) => {
    jsonpConvertRequest(r).flatMap(delegate.service).map(f => (e: E1) => f(e).map(jsonpConvertResponse(_, r.parameters.get('callback))))
  }

  def metadata = None
}

object JsonpService extends AkkaDefaults {
  private implicit val M: Monad[Future] = new FutureMonad(defaultFutureDispatch)

  def jsonpConvertRequest[T](r: HttpRequest[T])(implicit fromString: String => T): Validation[NotServed, HttpRequest[T]] = {
    import blueeyes.json.JParser.parse
    import blueeyes.json.serialization.DefaultSerialization._
    import Bijection._

    r.parameters.get('callback) match {
      case Some(callback) if (r.method == HttpMethods.GET) =>
        if (r.content.isEmpty) {
          try {
            val methodStr = r.parameters.get('method).getOrElse("get").toUpperCase

            val method = HttpMethods.PredefinedHttpMethods.find(_.value == methodStr).getOrElse(HttpMethods.GET)
            val content = r.parameters.get('content).map(fromString)
            val headers = r.parameters.get('headers).map(parse _).map(_.deserialize[Map[String, String]]).getOrElse(Map.empty[String, String])

            r.copy(method = method, content = content, headers = r.headers ++ headers).success
          } catch {
            case e => DispatchError(HttpException(HttpStatusCodes.BadRequest, Option(e.getMessage).getOrElse(""))).failure
          }
        } else {
          DispatchError(HttpException(HttpStatusCodes.BadRequest, "JSONP requested but content body is non-empty")).failure
        }

      case Some(callback) =>
        DispatchError(HttpException(HttpStatusCodes.BadRequest, "JSONP requested but HTTP method is not GET")).failure

      case None => 
        r.success
    }
  }

  private def jsonpMetadata[A](r: HttpResponse[A]) = JObject(
    JField("headers", r.headers.raw.serialize) ::
    JField("status",
      JObject(
        JField("code",    r.status.code.toString.jv) ::
        JField("reason",  r.status.reason.jv) ::
        Nil
      )
    ) ::
    Nil
  ).renderCompact

  def jsonpConvertResponse[T](response: HttpResponse[T], callback: Option[String])(implicit fromString: String => T, semigroup: Semigroup[T]): HttpResponse[T] = {
    import Bijection._

    callback map { callback =>
      val meta = jsonpMetadata(response)
      response.copy(
        status = HttpStatus(OK),
        content = response.content map { content =>
                    fromString(callback + "(") |+| content |+| fromString("," + meta + ");")
                  } orElse {
                    Some(fromString(callback + "(undefined," + meta + ");"))
                  }, 
        headers = response.headers + `Content-Type`(MimeTypes.text/MimeTypes.javascript)
      )
    } getOrElse {
      response
    }
  }

  def jsonpChunkedResponse[T, U](r: HttpResponse[Chunk[U]], callback: Option[String])(implicit u2s: U => String, s2t: String => T): HttpResponse[Chunk[T]] = {
    import blueeyes.json.serialization.DefaultSerialization._
    import Bijection._

    callback map { callback =>
      val meta = jsonpMetadata(r)
      r.copy(
        status = HttpStatus(OK),
        content = r.content.map { 
                    case Left(data) => 
                      Left(s2t(callback + "(" + u2s(data) + "," + meta + ");"))

                    case Right(stream) => 
                      val prefix = s2t(callback + "(")
                      val suffix = s2t("," + meta + ");")
                      Right((prefix :: stream.map(u2s andThen s2t)) ++ (suffix :: StreamT.empty[Future, T]))

                  } orElse {
                    Some(Left(s2t(callback + "(undefined," + meta + ");")))
                  }, 
        headers = r.headers + `Content-Type`(MimeTypes.text/MimeTypes.javascript)
      )
    } getOrElse {
      r.copy(
        content = r.content.map(_ map (u2s andThen s2t)),
        headers = r.headers + `Content-Type`(MimeTypes.application/MimeTypes.json)
      )
    }
  }
}


class ForwardingService[T, U](f: HttpRequest[T] => Option[HttpRequest[U]], httpClient: HttpClient[U], val delegate: HttpService[T, HttpResponse[T]]) 
extends DelegatingService[T, HttpResponse[T], T, HttpResponse[T]] with AkkaDefaults {
  def service = { r: HttpRequest[T] =>
    Future(f(r).foreach(httpClient))
    delegate.service(r)
  }

  def metadata = None
}

class ParameterService[T, S](parameter: Symbol, default: => Option[String], val delegate: HttpService[T, String => S])
extends DelegatingService[T, S, T, String => S]{
  def service = (r: HttpRequest[T]) => {
    r.parameters.get(parameter).orElse(default)
    .toSuccess(DispatchError(BadRequest, "Parameter " + parameter + " is required."))
    .flatMap(value => delegate.service(r.copy(parameters = r.parameters + (parameter -> value))).map(_.apply(value)))
  }

  lazy val metadata = Some(ParameterMetadata(parameter, default))
}

object ParameterService {
  def apply[T, S](parameter: Symbol, default: => Option[String], delegate: HttpService[T, String => S]) =
    new ParameterService(parameter, default, delegate)
}

class PathParameterService[A, B](path: RestPathPattern, sym: Symbol, val delegate: HttpService[A, String => B])
extends DelegatingService[A, B, A, String => B]{
  val service = (req: HttpRequest[A]) => {
    for { 
      request <- PathService.shift(path, req).toSuccess(inapplicable)
      value   <- request.parameters.get(sym).toSuccess(DispatchError(BadRequest, "The path element " + sym + " in " + path.toString + " was not found."))
      result  <- delegate.service(request)
    } yield {
      result(value)
    }
  }

  lazy val metadata = Some(PathPatternMetadata(path))
}

class PathDataService[A, B, X](path: RestPathPattern, sym: Symbol, f: String => Validation[NotServed, X], val delegate: HttpService[A, X => B])
extends DelegatingService[A, B, A, X => B]{
  val service = (req: HttpRequest[A]) => {
    for { 
      request  <- PathService.shift(path, req).toSuccess(inapplicable)
      valueStr <- request.parameters.get(sym).toSuccess(DispatchError(BadRequest, "The path element " + sym + " in " + path.toString + " was not found."))
      value    <- f(valueStr)
      result   <- delegate.service(request)
    } yield {
      result(value)
    }
  }

  lazy val metadata = Some(PathPatternMetadata(path))
}

class ExtractService[T, S, P](extractor: HttpRequest[T] => P, val delegate: HttpService[T, P => S]) extends DelegatingService[T, S, T, P => S]{
  def service = (r: HttpRequest[T]) => delegate.service(r).map(_.apply(extractor(r)))

  val metadata = None
}

class CookieService[A, B](ident: Symbol, default: => Option[String], val delegate: HttpService[A, String => B])
extends DelegatingService[A, B, A, String => B] {
  val service = (req: HttpRequest[A]) => {
    extractCookie(req, ident, default).toSuccess(DispatchError(BadRequest, "No cookie was found for the identifier " + ident))
    .flatMap(cookieValue => delegate.service(req).map(_.apply(cookieValue)))
  }

  def extractCookie[T](request: HttpRequest[T], ident: Symbol, default: => Option[String]): Option[String] = {
    val cookieValues = for (HttpHeaders.Cookie(cookies) <- request.headers.raw; cookie <- cookies if cookie.name == ident.name) yield cookie.cookieValue
    cookieValues.headOption.orElse(default)
  }

  lazy val metadata = Some(CookieMetadata(ident, default))
}

object CookieService {
  def apply[A, B](ident: Symbol, default: => Option[String], delegate: HttpService[A, String => B]) =
    new CookieService(ident, default, delegate)
}

class MetadataService[T, S](val metadata: Option[Metadata], val delegate: HttpService[T, S]) extends DelegatingService[T, S, T, S]{
  def service = delegate.service
}

class DecodeUrlService[T, S](val delegate: HttpService[T, S]) extends DelegatingService[T, S, T, S]{
  def service = (r: HttpRequest[T]) => delegate.service(r.copy(uri = r.uri.decode))

  val metadata = None
}

// type HttpServices //ctags help
