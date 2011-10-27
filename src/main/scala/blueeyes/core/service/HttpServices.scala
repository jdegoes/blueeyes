package blueeyes.core.service

import blueeyes.core.data._
import blueeyes.core.http.HttpHeaders.{`Content-Type`, `Accept-Encoding`}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import blueeyes.core.data.{ByteChunk, Bijection, AggregatedByteChunk, ZLIBByteChunk, GZIPByteChunk, CompressedByteChunk}
import blueeyes.util.metrics.DataSize
import blueeyes.util.printer._
import blueeyes.json.JsonAST._

import java.net.URLDecoder._

import scalaz.Scalaz._
import scalaz.{Validation, Failure}
import blueeyes.core.http._

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

trait DelegatingService[A, B, C, D] extends HttpService[A, B] {
  val delegate: HttpService[C, D]
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

case class HttpHandlerService[A, B](h: HttpServiceHandler[A, B]) extends CustomHttpService[A, B] {
  val service = (r: HttpRequest[A]) => success(h(r))

  val metadata = None
}

case class FailureService[A, B](onFailure: HttpRequest[A] => (HttpFailure, String)) extends CustomHttpService[A, B] {
  val service = (r: HttpRequest[A]) => DispatchError(onFailure(r)).fail[B]

  val metadata = None
}

case class PathService[A, B](path: RestPathPattern, delegate: HttpService[A, B], desc: Option[String]) extends DelegatingService[A, B, A, B] {
  val service = PathService.shift(path, _: HttpRequest[A]).toSuccess(inapplicable).flatMap(delegate.service)

  lazy val metadata = Some(PathPatternMetadata(path, desc))
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

case class HttpMethodService[A, B](method: HttpMethod, delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
  val service = (r: HttpRequest[A]) => if (r.method == method) delegate.service(r) else inapplicable.fail[B]

  lazy val metadata = Some(HttpMethodMetadata(method))
}

case class CommitService[A, B](delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
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
      
      DispatchError(NotFound, "No service was found to be able to handle your request. Were you trying to access (one of) the following? \n" + 
                    metadata.map(SimpleStringPrinter.printFormatted[Metadata]).mkString("\n")).fail
      
    case other => other
  }

  val metadata = None
}

case class AcceptService[T, S, U](mimeType: MimeType, delegate: HttpService[Future[T], Future[HttpResponse[S]]])(implicit b: Bijection[U, Future[T]]) extends DelegatingService[U, Future[HttpResponse[S]], Future[T], Future[HttpResponse[S]]] {
  import AcceptService._
  def service = (r: HttpRequest[U]) => convert(mimeType, r, inapplicable).flatMap{newRequest: HttpRequest[Future[T]] => delegate.service(newRequest).map{checkConvert(newRequest, _)} }

  lazy val metadata = Some(HeaderMetadata(Right(`Content-Type`(mimeType))))
}

case class Accept2Service[T, S, U, E1](mimeType: MimeType, delegate: HttpService[Future[T], E1 => Future[HttpResponse[S]]])(implicit b: Bijection[U, Future[T]]) extends DelegatingService[U, E1 => Future[HttpResponse[S]], Future[T], E1 => Future[HttpResponse[S]]] {
  import AcceptService._
  def service = (r: HttpRequest[U]) => convert(mimeType, r, inapplicable).flatMap {newRequest: HttpRequest[Future[T]] =>
    delegate.service(newRequest).map(function => (e: E1) => function.apply(e))
  }

  lazy val metadata = Some(HeaderMetadata(Right(`Content-Type`(mimeType))))
}

object AcceptService {
  def convert[U, T](mimeType: MimeType, r: HttpRequest[U], inapplicable: => Inapplicable)(implicit b: Bijection[U, Future[T]]) = {
    r.mimeTypes.find(_ == mimeType).map(_ => r.copy(content = r.content.map(b)).success).getOrElse(inapplicable.fail)
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

case class ProduceService[T, S, V](mimeType: MimeType, delegate: HttpService[T, Future[HttpResponse[S]]])(implicit b: Bijection[S, V]) 
extends DelegatingService[T, Future[HttpResponse[V]], T, Future[HttpResponse[S]]] {
  def service = (r: HttpRequest[T]) => delegate.service(r).map { 
    _.map(r => r.copy(content = r.content.map(b), headers = r.headers + `Content-Type`(mimeType)))
  }

  lazy val metadata = Some(HeaderMetadata(Right(`Content-Type`(mimeType))))
}

case class Produce2Service[T, S, V, E1](mimeType: MimeType, delegate: HttpService[T, E1 => Future[HttpResponse[S]]])(implicit b: Bijection[S, V]) 
extends DelegatingService[T, E1 => Future[HttpResponse[V]], T, E1 => Future[HttpResponse[S]]] {
  def service = (r: HttpRequest[T]) => delegate.service(r).map {
    f => f andThen ((_: Future[HttpResponse[S]]).map(r => r.copy(content = r.content.map(b), headers = r.headers + `Content-Type`(mimeType))))
  }

  lazy val metadata = Some(HeaderMetadata(Right(`Content-Type`(mimeType))))
}

case class RangeHeaderValues(units: String, bounds: List[(Long, Long)])

case class GetRangeService[T, S](delegate: HttpService[T, RangeHeaderValues => S]) 
extends DelegatingService[T, S, T, RangeHeaderValues => S] {
  val service = (req: HttpRequest[T]) => extractRange(req.headers.raw).flatMap(t => delegate.service(req).map(_.apply(t)))

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
              DispatchError(BadRequest, "The following range header values were incorrectly formatted: " + badValues.mkString(",")).fail
            }

          case _ => DispatchError(BadRequest, "The range header value was incorrectly formatted: " + value).fail
        }
    }
    
    if (matchingHeaders.size > 1) {
      DispatchError(BadRequest, "Multiple range header fields specified; please only specify one header of each type.").fail
    } else {
      matchingHeaders.headOption.getOrElse(inapplicable.fail)
    }
  }

  lazy val metadata = Some(HeaderMetadata(Left(HttpHeaders.Range), None, Some("A numeric range must be specified for the request.")))
}

case class CompressService(delegate: HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]])(implicit supportedCompressions: Map[Encoding, CompressedByteChunk]) extends DelegatingService[ByteChunk, Future[HttpResponse[ByteChunk]], ByteChunk, Future[HttpResponse[ByteChunk]]]{
  import CompressService._
  def service = (r: HttpRequest[ByteChunk]) => delegate.service(r).map{compress(r, _)}

  val metadata = Some(EncodingMetadata(supportedCompressions.keys.toSeq: _*))
}

case class CompressService2[E1](delegate: HttpService[ByteChunk, E1 => Future[HttpResponse[ByteChunk]]])(implicit supportedCompressions: Map[Encoding, CompressedByteChunk]) extends DelegatingService[ByteChunk, E1 => Future[HttpResponse[ByteChunk]], ByteChunk, E1 => Future[HttpResponse[ByteChunk]]]{
  import CompressService._
  def service = (r: HttpRequest[ByteChunk]) => delegate.service(r).map{function => (e: E1) => compress(r, function.apply(e))}

  val metadata = Some(EncodingMetadata(supportedCompressions.keys.toSeq: _*))
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

case class AggregateService(chunkSize: Option[DataSize], delegate: HttpService[Future[ByteChunk], Future[HttpResponse[ByteChunk]]]) 
extends DelegatingService[ByteChunk, Future[HttpResponse[ByteChunk]], Future[ByteChunk], Future[HttpResponse[ByteChunk]]]{
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

case class JsonpService[T](delegate: HttpService[Future[JValue], Future[HttpResponse[JValue]]])(implicit b1: Bijection[T, Future[JValue]], bstr: Bijection[T, String]) 
extends DelegatingService[T, Future[HttpResponse[T]], Future[JValue], Future[HttpResponse[JValue]]]{
  import JsonpService._
  def service = (r: HttpRequest[T]) => jsonpConvertRequest(r).flatMap(delegate.service(_)).map(_.map(jsonpConvertResponse(_, r.parameters.get('callback))))

  val metadata = Some(OrMetadata(
    AndMetadata(
      HttpMethodMetadata(HttpMethods.GET),
      ParameterMetadata('callback, None, Some("A callback method identifier is required when using JsonP with a \"GET\" request."))
    ),
    HttpMethodMetadata(HttpMethods.POST),
    HttpMethodMetadata(HttpMethods.PUT),
    HttpMethodMetadata(HttpMethods.DELETE)
  ))
}

case class Jsonp2Service[T, E1](delegate: HttpService[Future[JValue], E1 => Future[HttpResponse[JValue]]])(implicit b1: Bijection[T, Future[JValue]], bstr: Bijection[T, String]) 
extends DelegatingService[T, E1 => Future[HttpResponse[T]], Future[JValue], E1 => Future[HttpResponse[JValue]]]{
  import JsonpService._
  def service = (r: HttpRequest[T]) => {
    jsonpConvertRequest(r).flatMap(delegate.service).map(f => (e: E1) => f(e).map(jsonpConvertResponse(_, r.parameters.get('callback))))
  }

  def metadata = None
}

object JsonpService {
  def jsonpConvertRequest[T](r: HttpRequest[T])(implicit b1: Bijection[T, Future[JValue]]): Validation[NotServed, HttpRequest[Future[JValue]]] = {
    import blueeyes.json.JsonParser.parse
    import blueeyes.json.xschema.DefaultSerialization._
    import Bijection._

    r.parameters.get('callback) match {
      case Some(callback) if (r.method == HttpMethods.GET) =>
        if (r.content.isEmpty) {
          try {
            val methodStr = r.parameters.get('method).getOrElse("get").toUpperCase

            val method = HttpMethods.PredefinedHttpMethods.find(_.value == methodStr).getOrElse(HttpMethods.GET)
            val content = r.parameters.get('content).map(parse _).map(Future.sync(_))
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
        success(r.copy(content = r.content.map(_.as[Future[JValue]])))
    }
  }

  def jsonpConvertResponse[T](r: HttpResponse[JValue], callback: Option[String])(implicit b1: Bijection[T, Future[JValue]], bstr: Bijection[T, String]): HttpResponse[T] = {
    import blueeyes.json.xschema.DefaultSerialization._
    import blueeyes.json.Printer._
    import Bijection._

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
          bstr.inverse.apply(callback + "(" + bstr.apply(Future.sync(content).as[T]) + "," + meta + ");")
        } orElse {
          Some(bstr.inverse.apply(callback + "(undefined," + meta + ");"))
        }, 
        headers = r.headers + `Content-Type`(MimeTypes.text/MimeTypes.javascript)
      )
    } getOrElse {
      r.copy(content = r.content.map(Future.sync(_).as[T]), headers = r.headers + `Content-Type`(MimeTypes.application/MimeTypes.json))
    }
  }
}


case class ForwardingService[T, U](f: HttpRequest[T] => Option[HttpRequest[U]], httpClient: HttpClient[U], delegate: HttpService[T, HttpResponse[T]]) 
extends DelegatingService[T, HttpResponse[T], T, HttpResponse[T]]{
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

case class PathParameterService[A, B](path: RestPathPattern, sym: Symbol, desc: Option[String], delegate: HttpService[A, String => B]) 
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

case class PathDataService[A, B, X](path: RestPathPattern, sym: Symbol, f: String => Validation[NotServed, X], desc: Option[String], delegate: HttpService[A, X => B]) 
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

  lazy val metadata = Some(PathPatternMetadata(path, desc))
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
