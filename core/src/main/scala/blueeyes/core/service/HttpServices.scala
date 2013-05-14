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
import blueeyes.health.HealthMonitor
import blueeyes.json._
import blueeyes.json.serialization.DefaultSerialization._
import blueeyes.util.metrics.DataSize
import blueeyes.util.printer._
import Metadata._

import java.net.URLDecoder._

import scalaz._
import scalaz.syntax.functor._
import scalaz.syntax.kleisli._
import scalaz.syntax.show._
import scalaz.syntax.semigroup._
import scalaz.syntax.validation._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._

import com.weiglewilczek.slf4s.Logger

sealed trait AnyService {
  def metadata: Metadata
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

  def withMetadata(m: Metadata) = new MetadataService(m, this)
}

object HttpService {
  implicit def cov[A]: Functor[({ type l[b] = HttpService[A, b] })#l] = {
    type SF[B] = HttpService[A, B]
    new Functor[SF] {
      def map[B0, B](fa: SF[B0])(f: B0 => B): SF[B] = fa.map(f)
    }
  }

  implicit def con[B]: Contravariant[({ type l[a] = HttpService[a, B] })#l] = {
    type SF[A] = HttpService[A, B]
    new Contravariant[SF] {
      def contramap[A0, A](fa: SF[A0])(f: A => A0): SF[A] = fa.contramap(f)
    }
  }
}

/**
 * An open base trait for HTTP services that allows extension beyond
 * the sealed hierarchy.
 */
trait CustomHttpService[A, B] extends HttpService[A, B]

trait DelegatingService[A, B, A0, B0] extends HttpService[A, B] {
  val delegate: HttpService[A0, B0]
}

object DelegatingService {
  def unapply[A, B, C, D](s: DelegatingService[A, B, C, D]): Option[HttpService[C, D]] = Some(s.delegate)
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

  val metadata = NoMetadata
}


/**
 * A higher-order natural transformation on responses that allows uniform handling
 * for any 
 */
trait ResponseModifier[A] {
  def modify(result: A)(f: HttpResponse ~> HttpResponse): A
}

object ResponseModifier {
  // because automatically lifting to the identity functor isn't likely to work
  implicit def response[A]: ResponseModifier[HttpResponse[A]] = new ResponseModifier[HttpResponse[A]] {
    def modify(result: HttpResponse[A])(f: HttpResponse ~> HttpResponse) = f[A](result)
  }

  implicit def responseF[F[_]: Functor, A]: ResponseModifier[F[HttpResponse[A]]] = new ResponseModifier[F[HttpResponse[A]]] {
    def modify(result: F[HttpResponse[A]])(f: HttpResponse ~> HttpResponse) = result map { r => f[A](r) }
  }

  implicit def responseFG[F[_]: Functor, G[_]: Functor, A]: ResponseModifier[F[G[HttpResponse[A]]]] = new ResponseModifier[F[G[HttpResponse[A]]]] {
    def modify(result: F[G[HttpResponse[A]]])(f: HttpResponse ~> HttpResponse) = result map { _ map { r => f[A](r) } }
  }
}

////////////////////////////////////////////////////
// Handlers that are descendents of the ADT types //
////////////////////////////////////////////////////

class HttpHandlerService[A, B](h: HttpRequest[A] => B) extends CustomHttpService[A, B] {
  val service = (r: HttpRequest[A]) => h(r).success

  val metadata = NoMetadata
}

class FailureService[A, B](onFailure: HttpRequest[A] => (HttpFailure, String)) extends CustomHttpService[A, B] {
  val service = (r: HttpRequest[A]) => {
    val (errorCode, message) = onFailure(r)
    DispatchError(errorCode, message, Some(r.shows)).failure[B]
  }

  val metadata = DescriptionMetadata("This service will always return a DispatchError.")
}

class DebugService[A, B](logger: Logger, val delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
  val service = (request: HttpRequest[A]) => {
    println("Received request: " + request)
    logger.debug("Received request: " + request)
    delegate.service(request)
  }

  val metadata = NoMetadata
}

class IfRequestService[A, B](p: HttpRequest[A] => Boolean, val delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
  val service = (req: HttpRequest[A]) => {
    if (p(req)) delegate.service(req) else Failure(inapplicable)
  }

  val metadata = NoMetadata
}


class HealthMonitorService[A, B](context: ServiceContext, monitor: HealthMonitor, startTime: Long)(implicit jv2b: JValue => B) extends CustomHttpService[A, Future[HttpResponse[B]]] {
  val service = (req: HttpRequest[A]) => Success {
    import scalaz.syntax.show._
    for (requests <- monitor.toJValue) yield {
      val version       = context.serviceVersion
      val responseContent = JObject(
        "service" -> JObject(
          "name" -> JString(context.serviceName),
          "version" -> JString("%d.%d.%s".format(version.majorVersion, version.minorVersion, version.version))
        ),
        "server" -> JObject(
          "hostName" -> JString(context.hostName),
          "port" -> JNum(context.port),
          "sslPort" -> JNum(context.sslPort)
        ),
        "uptimeSeconds" -> JNum((System.currentTimeMillis - startTime) / 1000),
        "requests" -> requests
      )

      HttpResponse[B](content = Some(jv2b(responseContent)))
    }
  }

  val metadata = DescriptionMetadata(
    """Exports real-time metrics on health status, for use in continuous deployment. The default health monitor automatically exports information on number of requests, number and type of errors, and length of requests"""
  )
}

class PathService[A, B](path: RestPathPattern, val delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
  val service = PathService.shift(path, _: HttpRequest[A]).toSuccess(inapplicable).flatMap(delegate.service)

  val metadata = PathPatternMetadata(path)
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

  val metadata = HttpMethodMetadata(method)
}

class CommitService[A, B](val delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
  def pathMetadata(service: AnyService, services: Set[AnyService], path: Vector[Metadata]): Vector[Vector[Metadata]] = {
    if (services.contains(service)) Vector(path)
    else service match {
      case OrService(orMembers @ _*) => orMembers.flatMap((s: AnyService) => pathMetadata(s, services, path :+ s.metadata))(collection.breakOut)
      case DelegatingService(child)  => pathMetadata(child, services, path :+ child.metadata)
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

  val metadata = NoMetadata
}

class TranscodeService[A, B](val delegate: HttpService[Future[B], Future[HttpResponse[B]]])(implicit inj: A => Future[B], surj: B => A)
extends DelegatingService[A, Future[HttpResponse[A]], Future[B], Future[HttpResponse[B]]] {
  val service = delegate.contramap(inj).map(_ map { _ map surj }).service
  val metadata = NoMetadata
}

class AcceptService[A, B](mimeTypes: Seq[MimeType], val delegate: HttpService[A, B]) extends DelegatingService[A, B, A, B] {
  val service = (r: HttpRequest[A]) => r.mimeTypes.exists(mimeTypes.toSet).option(r).toSuccess(inapplicable) flatMap { delegate.service }
  val metadata = RequestHeaderMetadata(Right(`Content-Type`(mimeTypes: _*)))
}

class ProduceService[A, B](mimeType: MimeType, val delegate: HttpService[A, B], modifier: ResponseModifier[B]) extends DelegatingService[A, B, A, B] {
  import HttpHeaders.Accept
  def service = (r: HttpRequest[A]) => {
    val acceptHeader = r.headers.header[Accept].orElse(Some(Accept(mimeType)))
    acceptHeader.flatMap(_.mimeTypes.find(mimeType.satisfiesRequestFor)).toSuccess(inapplicable) flatMap { accepted =>
      delegate.map(b => modifier.modify(b) { HttpResponse.modifyHeaders(_ + `Content-Type`(mimeType)) }).service(r)
    }
  }

  val metadata = ResponseHeaderMetadata(Right(`Content-Type`(mimeType)))
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

  val metadata = AndMetadata(
    RequestHeaderMetadata(Left(HttpHeaders.Range), None),
    DescriptionMetadata("A numeric range must be specified for the request.")
  )
}

class AggregateService(chunkSize: Option[DataSize], val delegate: HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]])(implicit executor: ExecutionContext)
extends DelegatingService[ByteChunk, Future[HttpResponse[ByteChunk]], ByteChunk, Future[HttpResponse[ByteChunk]]]{
  private val size = chunkSize.map(_.intBytes).getOrElse(ByteChunk.defaultChunkSize)

  def service = (r: HttpRequest[ByteChunk]) => {
    delegate.service(r.copy(content = r.content.map(ByteChunk.aggregate(_, size))))
  }

  val metadata = opt2M(chunkSize.map(DataSizeMetadata))
}

class JsonpService[A, B](val delegate: HttpService[A, Future[HttpResponse[B]]])(implicit extractReq: String => A, extractResp: String => B, semigroup: Semigroup[B])
extends DelegatingService[A, Future[HttpResponse[B]], A, Future[HttpResponse[B]]]{
  import JsonpService._
  def service = (r: HttpRequest[A]) => jsonpConvertRequest(r).flatMap(delegate.service).map(_.map(jsonpConvertResponse(_, r.parameters.get('callback))))

  val metadata = JsonpService.metadata
}

object JsonpService extends AkkaDefaults {
  private implicit val M: Monad[Future] = new FutureMonad(defaultFutureDispatch)

  val metadata = OrMetadata(
    AndMetadata(
      HttpMethodMetadata(HttpMethods.GET),
      DescriptionMetadata("A callback method identifier is required when using JsonP with a \"GET\" request."),
      ParameterMetadata('callback, None)
    ),
    HttpMethodMetadata(HttpMethods.POST),
    HttpMethodMetadata(HttpMethods.PUT),
    HttpMethodMetadata(HttpMethods.DELETE)
  )

  val reservedParameters = List('method, 'content, 'headers, 'query)

  def jsonpConvertRequest[T](r: HttpRequest[T])(implicit fromString: String => T): Validation[NotServed, HttpRequest[T]] = {
    import blueeyes.json.JParser.validateFromString
    import blueeyes.json.serialization.Extractor
    import blueeyes.json.serialization.DefaultSerialization._
    import blueeyes.util.QueryParser.parseQuery
    import scalaz.Validation._
    import scalaz.syntax.bifunctor._
    import HttpStatusCodes._
    import Bijection._

    def parseFailure(err: Extractor.Error) = DispatchError(BadRequest, "Errors encountered parsing JSON-encoded headers.", Some(err.message))

    r.parameters.get('callback) match {
      case Some(callback) if (r.method == HttpMethods.GET) =>
        if (r.content.isEmpty) {
          val methodStr = r.parameters.get('method).getOrElse("get").toUpperCase

          val method = HttpMethods.PredefinedHttpMethods.find(_.value == methodStr).getOrElse(HttpMethods.GET)
          val content = r.parameters.get('content).map(fromString)
          val headers = (parseFailure _) <-: r.parameters.get('headers).map(validateFromString[Map[String, String]]).getOrElse(success(Map()))
          val parameters = (r.parameters -- reservedParameters) ++ r.parameters.get('query).map(parseQuery).getOrElse(Map.empty[Symbol, String])

          headers map { headers0 =>
            r.copy(method = method, headers = r.headers ++ headers0, parameters = parameters, content = content)
          }
        } else {
          DispatchError(BadRequest, "JSONP requested but content body is non-empty").failure
        }

      case Some(callback) =>
        DispatchError(BadRequest, "JSONP requested but HTTP method is not GET").failure

      case None =>
        r.success
    }
  }

  private def jsonpMetadata[A](r: HttpResponse[A]) = JObject(
    "headers" -> r.headers.raw.jv,
    "status" -> JObject(
      "code" -> r.status.code.value.jv,
      "reason" -> r.status.reason.jv
    )
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

class ProxyService[A](httpClient: HttpClient[A], filter: HttpRequest[A] => Boolean) 
extends CustomHttpService[A, Future[HttpResponse[A]]] {
  def service = { r: HttpRequest[A] => 
    if (filter(r) && httpClient.isDefinedAt(r)) {
      Success(httpClient(r))
    } else {
      Failure(inapplicable)
    }
  }

  val metadata = NoMetadata
}


class ForwardingService[T, U](f: HttpRequest[T] => Option[HttpRequest[U]], httpClient: HttpClient[U], val delegate: HttpService[T, HttpResponse[T]])
extends DelegatingService[T, HttpResponse[T], T, HttpResponse[T]] with AkkaDefaults {
  def service = { r: HttpRequest[T] =>
    Future(f(r).foreach(httpClient))
    delegate.service(r)
  }

  val metadata = NoMetadata
}

class ParameterService[T, S](parameter: Symbol, default: => Option[String], val delegate: HttpService[T, String => S])
extends DelegatingService[T, S, T, String => S]{
  def service = (r: HttpRequest[T]) => {
    r.parameters.get(parameter).orElse(default)
    .toSuccess(DispatchError(BadRequest, "Parameter " + parameter + " is required."))
    .flatMap(value => delegate.service(r.copy(parameters = r.parameters + (parameter -> value))).map(_.apply(value)))
  }

  val metadata = ParameterMetadata(parameter, default)
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

  val metadata = PathPatternMetadata(path)
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

  val metadata = PathPatternMetadata(path)
}

class ExtractService[T, S, P](extractor: HttpRequest[T] => P, val delegate: HttpService[T, P => S]) extends DelegatingService[T, S, T, P => S]{
  def service = (r: HttpRequest[T]) => delegate.service(r).map(_.apply(extractor(r)))
  val metadata = NoMetadata
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

  val metadata = CookieMetadata(ident, default)
}

object CookieService {
  def apply[A, B](ident: Symbol, default: => Option[String], delegate: HttpService[A, String => B]) =
    new CookieService(ident, default, delegate)
}

class MetadataService[T, S](val metadata: Metadata, val delegate: HttpService[T, S]) extends DelegatingService[T, S, T, S]{
  def service = delegate.service
}

class DecodeUrlService[T, S](val delegate: HttpService[T, S]) extends DelegatingService[T, S, T, S]{
  def service = (r: HttpRequest[T]) => delegate.service(r.copy(uri = r.uri.decode))
  val metadata = NoMetadata
}

// type HttpServices //ctags help
