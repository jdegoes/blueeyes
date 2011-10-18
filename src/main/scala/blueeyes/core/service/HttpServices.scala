package blueeyes.core.service

import blueeyes.core.data._
import blueeyes.core.http.HttpHeaders.{`Content-Type`, `Accept-Encoding`}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import blueeyes.core.data.{ByteChunk, Bijection, AggregatedByteChunk, ZLIBByteChunk, GZIPByteChunk, CompressedByteChunk}
import blueeyes.util.metrics.DataSize
import blueeyes.json.JsonAST._

import java.net.URLDecoder._

import scala.annotation.tailrec
import scalaz.Scalaz._
import scalaz.{Success, Validation, Failure}
import blueeyes.core.http._

sealed trait NotServed {
  def or[A](result: => Validation[NotServed, A]): Validation[NotServed, A]
}

case class DispatchError(exception: HttpException) extends NotServed {
  override def or[A](result: => Validation[NotServed, A]) = this.fail[A]
}

object DispatchError {
  def apply(failure: HttpFailure, reason: String): DispatchError = DispatchError(HttpException(failure, reason))
}

case object Inapplicable extends NotServed {
  override def or[A](result: => Validation[NotServed, A]) = result
}

sealed trait Metadata 

case class DataSizeMetadata     (dataSize: DataSize) extends Metadata
case class HeaderMetadata       (mimeType: HttpHeader, desc: Option[String] = None) extends Metadata
case class PathPatternMetadata  (pattern: RestPathPattern, desc: Option[String] = None) extends Metadata
case class ParameterMetadata    (parameter: Symbol, default: Option[String], desc: Option[String] = None) extends Metadata
case class CookieMetadata       (ident: Symbol, default: Option[String], desc: Option[String] = None) extends Metadata
case class HttpMethodMetadata   (method: HttpMethod) extends Metadata
case class DescriptionMetadata  (description: String) extends Metadata
case class OrMetadata           (metadata: Metadata*) extends Metadata
case class AndMetadata          (metadata: Metadata*) extends Metadata

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

  def ~ (other: HttpService[A, B]) = OrService(self, other)
  def ~ [C, D](other: HttpService[C, D])(implicit unapply: Unapply[C, A], apply: D => B) = OrService(self, other.contramap(unapply.unapply).map(apply))

  def metadata: Option[Metadata]
}

trait CustomHttpService[A, B] extends HttpService[A, B] {
  def metadata = None
}

case class OrService[A, B](services: HttpService[A, B]*) extends HttpService[A, B] {
  /*@tailrec*/ private def pick(r: HttpRequest[A], services: Seq[HttpService[A, B]]): Validation[NotServed, B] = services.headOption match {
    case None => Inapplicable.fail
    case Some(service) =>
      val ss = service
      service.service(r) match {
      case success: scalaz.Success[_, _] => success
      case Failure(notServed) => notServed or pick(r, services.tail)
    }
  }
 
  val service = (r: HttpRequest[A]) => pick(r, services)

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

case class HttpHandlerService[A, B](h: HttpServiceHandler[A, B]) extends HttpService[A, B]{
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

case class AcceptService[T, S, U](mimeType: MimeType, delegate: HttpService[Future[T], Future[HttpResponse[S]]])(implicit b: Bijection[U, Future[T]]) extends DelegatingService[U, Future[HttpResponse[S]], Future[T], Future[HttpResponse[S]]] {
  import AcceptService._
  def service = (r: HttpRequest[U]) => convert(mimeType, r).flatMap{newRequest: HttpRequest[Future[T]] => delegate.service(newRequest).map{checkConvert(newRequest, _)} }

  lazy val metadata = Some(HeaderMetadata(`Content-Type`(mimeType)))
}

case class Accept2Service[T, S, U, E1](mimeType: MimeType, delegate: HttpService[Future[T], E1 => Future[HttpResponse[S]]])(implicit b: Bijection[U, Future[T]]) extends DelegatingService[U, E1 => Future[HttpResponse[S]], Future[T], E1 => Future[HttpResponse[S]]] {
  import AcceptService._
  def service = (r: HttpRequest[U]) => convert(mimeType, r).flatMap {newRequest: HttpRequest[Future[T]] =>
    delegate.service(newRequest).map(function => (e: E1) => function.apply(e))
  }

  lazy val metadata = Some(HeaderMetadata(`Content-Type`(mimeType)))
}

object AcceptService {
  def convert[U, T](mimeType: MimeType, r: HttpRequest[U])(implicit b: Bijection[U, Future[T]]) = {
    r.mimeTypes.find(_ == mimeType).map(_ => r.copy(content = r.content.map(b)).success).getOrElse(Inapplicable.fail)
  }

  def checkConvert[T, S](request: HttpRequest[Future[T]], response: Future[HttpResponse[S]]) = {
    request.content.map{content =>
      val result = new Future[HttpResponse[S]]
      content  ifCanceled{error => result.deliver(HttpResponse[S](status = HttpStatus(BadRequest, error.flatMap(value => Option(value.getMessage)).getOrElse(""))))}
      response ifCanceled(result.cancel(_))
      response deliverTo(result.deliver(_))
      result
    }.getOrElse(response)
  }
}

case class ProduceService[T, S, V](mimeType: MimeType, delegate: HttpService[T, Future[HttpResponse[S]]])(implicit b: Bijection[S, V]) extends DelegatingService[T, Future[HttpResponse[V]], T, Future[HttpResponse[S]]]{
  def service = (r: HttpRequest[T]) => delegate.service(r).map { 
    _.map(response => response.copy(content = response.content.map(b), headers = response.headers + `Content-Type`(mimeType)))
  }

  lazy val metadata = Some(HeaderMetadata(`Content-Type`(mimeType)))
}

case class Produce2Service[T, S, V, E1](mimeType: MimeType, delegate: HttpService[T, E1 => Future[HttpResponse[S]]])(implicit b: Bijection[S, V]) extends DelegatingService[T, E1 => Future[HttpResponse[V]], T, E1 => Future[HttpResponse[S]]]{
  def service = (r: HttpRequest[T]) => delegate.service(r).map {
    f => f andThen ((r: Future[HttpResponse[S]]) => r.map(response => response.copy(content = response.content.map(b), headers = response.headers + `Content-Type`(mimeType))))
  }

  lazy val metadata = Some(HeaderMetadata(`Content-Type`(mimeType)))
}

case class GetRangeService[T, S](h: (List[(Long, Long)], String) => HttpServiceHandler[T, S]) extends HttpService[T, S]{
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

case class CompressService(delegate: HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]])(implicit supportedCompressions: Map[Encoding, CompressedByteChunk]) extends DelegatingService[ByteChunk, Future[HttpResponse[ByteChunk]], ByteChunk, Future[HttpResponse[ByteChunk]]]{
  import CompressService._
  def service = (r: HttpRequest[ByteChunk]) => delegate.service(r).map{compress(r, _)}

  val metadata = None
}

case class CompressService2[E1](delegate: HttpService[ByteChunk, E1 => Future[HttpResponse[ByteChunk]]])(implicit supportedCompressions: Map[Encoding, CompressedByteChunk]) extends DelegatingService[ByteChunk, E1 => Future[HttpResponse[ByteChunk]], ByteChunk, E1 => Future[HttpResponse[ByteChunk]]]{
  import CompressService._
  def service = (r: HttpRequest[ByteChunk]) => delegate.service(r).map{function => (e: E1) => compress(r, function.apply(e))}

  val metadata = None
}

object CompressService {
  def compress(r: HttpRequest[ByteChunk], f: Future[HttpResponse[ByteChunk]])(implicit supportedCompressions: Map[Encoding, CompressedByteChunk]) = f.map{response =>
    val encodings = r.headers.header(`Accept-Encoding`).map(_.encodings.toList).getOrElse(Nil)
    val supported = supportedCompressions.filterKeys(encodings.contains(_)).headOption.map(_._2)
    (supported, response.content) match {
      case (Some(compression), Some(content)) => response.copy(content = Some(compression(content)))
      case _ => response
    }
  }

  val supportedCompressions = Map[Encoding, CompressedByteChunk](Encodings.gzip -> GZIPByteChunk, Encodings.deflate -> ZLIBByteChunk)
}

case class AggregateService(chunkSize: Option[DataSize], delegate: HttpService[Future[ByteChunk], Future[HttpResponse[ByteChunk]]]) extends DelegatingService[ByteChunk, Future[HttpResponse[ByteChunk]], Future[ByteChunk], Future[HttpResponse[ByteChunk]]]{
  def service = (r: HttpRequest[ByteChunk]) => delegate.service(r.copy(content = r.content.map(AggregatedByteChunk(_, chunkSize))))

  lazy val metadata = chunkSize.map(DataSizeMetadata)
}

case class Aggregate2Service[E1](chunkSize: Option[DataSize], delegate: HttpService[Future[ByteChunk], E1 => Future[HttpResponse[ByteChunk]]]) 
extends DelegatingService[ByteChunk, E1 => Future[HttpResponse[ByteChunk]], Future[ByteChunk], E1 => Future[HttpResponse[ByteChunk]]]{
  def service = (r: HttpRequest[ByteChunk]) => {
    delegate.service(r.copy(content = r.content.map(AggregatedByteChunk(_, chunkSize)))).map(f => (e: E1) => f(e))
  }

  lazy val metadata = chunkSize.map(DataSizeMetadata)
}

case class JsonpService[T](delegate: HttpService[JValue, Future[HttpResponse[JValue]]])(implicit b1: Bijection[T, JValue], bstr: Bijection[T, String]) 
extends DelegatingService[T, Future[HttpResponse[T]], JValue, Future[HttpResponse[JValue]]]{
  import JsonpService._
  def service = (r: HttpRequest[T]) => jsonpConvertRequest(r).flatMap(delegate.service(_)).map(_.map(jsonpConvertResponse(_, r.parameters.get('callback))))

  def metadata = Some(OrMetadata(
    AndMetadata(
      HttpMethodMetadata(HttpMethods.GET),
      ParameterMetadata('callback, None, Some("A callback method identifier is required when using JsonP with a \"GET\" request."))
    ),
    HttpMethodMetadata(HttpMethods.POST),
    HttpMethodMetadata(HttpMethods.PUT),
    HttpMethodMetadata(HttpMethods.DELETE)
  ))
}

case class Jsonp2Service[T, E1](delegate: HttpService[JValue, E1 => Future[HttpResponse[JValue]]])(implicit b1: Bijection[T, JValue], bstr: Bijection[T, String]) 
extends DelegatingService[T, E1 => Future[HttpResponse[T]], JValue, E1 => Future[HttpResponse[JValue]]]{
  import JsonpService._
  def service = (r: HttpRequest[T]) => {
    jsonpConvertRequest(r).flatMap(delegate.service).map(f => (e: E1) => f(e).map(jsonpConvertResponse(_, r.parameters.get('callback))))
  }

  def metadata = None
}

object JsonpService {
  def jsonpConvertRequest[T](r: HttpRequest[T])(implicit b1: Bijection[T, JValue]): Validation[NotServed, HttpRequest[JValue]] = {
    import blueeyes.json.JsonParser.parse
    import blueeyes.json.xschema.DefaultSerialization._

    r.parameters.get('callback) match {
      case Some(callback) if (r.method == HttpMethods.GET) =>
        if (r.content.isEmpty) {
          try {
            val methodStr = r.parameters.get('method).getOrElse("get").toUpperCase

            val method = HttpMethods.PredefinedHttpMethods.find(_.value == methodStr).getOrElse(HttpMethods.GET)
            val content = r.parameters.get('content).map(parse _)
            val headers = r.parameters.get('headers).map(parse _).map(_.deserialize[Map[String, String]]).getOrElse(Map.empty[String, String])

            success(r.copy(method = method, content = content, headers = r.headers ++ headers))
          }
          catch {
            case e => failure(DispatchError(HttpException(HttpStatusCodes.BadRequest, Option(e.getMessage).getOrElse(""))))
          }
        } else {
          failure(DispatchError(HttpException(HttpStatusCodes.BadRequest, "JSONP requested but content body is non-empty")))
        }

      case Some(callback) =>
        failure(DispatchError(HttpException(HttpStatusCodes.BadRequest, "JSONP requested but HTTP method is not GET")))

      case None =>
        success(r.copy(content = r.content.map(b1.apply)))
    }
  }

  def jsonpConvertResponse[T](r: HttpResponse[JValue], callback: Option[String])(implicit b1: Bijection[T, JValue], bstr: Bijection[T, String]): HttpResponse[T] = {
    import blueeyes.json.xschema.DefaultSerialization._
    import blueeyes.json.Printer._

    implicit val b2 = b1.inverse

    callback map { callback =>
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

      r.copy(
        content = r.content.map { content =>
          bstr.inverse.apply(callback + "(" + bstr.apply(b2.apply(content)) + "," + meta + ");")
        } orElse {
          Some(bstr.inverse.apply(callback + "(undefined," + meta + ");"))
        }, 
        headers = r.headers + `Content-Type`(MimeTypes.text/MimeTypes.javascript)
      )
    } getOrElse {
      r.copy(content = r.content.map(b2.apply), headers = r.headers + `Content-Type`(MimeTypes.application/MimeTypes.json))
    }
  }
}


case class ForwardingService[T, U](f: HttpRequest[T] => Option[HttpRequest[U]], httpClient: HttpClient[U], delegate: HttpService[T, HttpResponse[T]]) extends DelegatingService[T, HttpResponse[T], T, HttpResponse[T]]{
  def service = { r: HttpRequest[T] =>
    Future.async(f(r).foreach(httpClient))
    delegate.service(r)
  }

  def metadata = None
}

class ParameterService[T, S](parameter: Symbol, default: => Option[String], desc: Option[String], val delegate: HttpService[T, String => S]) 
extends DelegatingService[T, S, T, String => S]{
  def service = (r: HttpRequest[T]) => {
    r.parameters.get(parameter).orElse(default)
    .toSuccess(DispatchError(BadRequest, "Parameter " + parameter + desc.map(" ("+_+")").getOrElse("") + " is required."))
    .flatMap(value => delegate.service(r.copy(parameters = r.parameters + (parameter -> value))).map(_.apply(value)))
  }

  lazy val metadata = Some(ParameterMetadata(parameter, default, desc))
}

object ParameterService {
  def apply[T, S](parameter: Symbol, default: => Option[String], desc: Option[String], delegate: HttpService[T, String => S]) = 
    new ParameterService(parameter, default, desc, delegate)
}

case class ExtractService[T, S, P](extractor: HttpRequest[T] => P, val delegate: HttpService[T, P => S]) extends DelegatingService[T, S, T, P => S]{
  def service = (r: HttpRequest[T]) => delegate.service(r).map(_.apply(extractor(r)))

  val metadata = None
}

class CookieService[A, B](ident: Symbol, default: => Option[String], desc: Option[String], val delegate: HttpService[A, String => B]) 
extends DelegatingService[A, B, A, String => B] {
  val service = (req: HttpRequest[A]) => {
    extractCookie(req, ident, default).toSuccess(DispatchError(BadRequest, "No cookie was found for the identifier " + ident))
    .flatMap(cookieValue => delegate.service(req).map(_.apply(cookieValue)))
  }

  def extractCookie[T](request: HttpRequest[T], ident: Symbol, default: => Option[String]): Option[String] = {
    val cookieValues = for (HttpHeaders.Cookie(cookies) <- request.headers.raw; cookie <- cookies if cookie.name == ident.name) yield cookie.cookieValue
    cookieValues.headOption.orElse(default)
  }

  lazy val metadata = Some(CookieMetadata(ident, default, desc))
}

object CookieService {
  def apply[A, B](ident: Symbol, default: => Option[String], desc: Option[String], delegate: HttpService[A, String => B]) =
    new CookieService(ident, default, desc, delegate)
}

case class MetadataService[T, S](metadata: Option[Metadata], delegate: HttpService[T, S]) extends DelegatingService[T, S, T, S]{
  def service = delegate.service
}

case class DecodeUrlService[T, S](delegate: HttpService[T, S]) extends DelegatingService[T, S, T, S]{
  def service = (r: HttpRequest[T]) => delegate.service(r.copy(uri = r.uri.decode))

  val metadata = None
}
