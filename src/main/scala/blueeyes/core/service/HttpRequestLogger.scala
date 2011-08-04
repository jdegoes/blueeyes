package blueeyes.core.service

import java.net.InetAddress

import blueeyes.core.http.{HttpRequest, HttpResponse, HttpHeaders}
import blueeyes.util.Clock
import blueeyes.concurrent.Future

import org.joda.time.format.DateTimeFormat
import org.apache.commons.codec.binary.Base64
import blueeyes.core.data.{AggregatedByteChunk, Bijection, ByteChunk}

/** A request logger is a function from (request/future of response) to future
 * of log line. Request loggers do not have side effects.
 */
trait HttpRequestLogger[T, S] extends ((HttpRequest[T], Future[HttpResponse[S]]) => Future[String]) { self =>
  /** Combines this logger with the specified logger to produce another logger.
   * If necessary, the items are separated by a single space character.
   */
  def :+ (logger: HttpRequestLogger[T, S]): HttpRequestLogger[T, S] = new CompositeHttpRequestLogger(logger)

  private[HttpRequestLogger] class CompositeHttpRequestLogger(logger: HttpRequestLogger[T, S]) extends HttpRequestLogger[T, S]{
    def apply(request: HttpRequest[T], response: Future[HttpResponse[S]]): Future[String] = {
      self(request, response).zip(logger(request, response)).map { t =>
        val (prefix, suffix) = t

        val infix = if (suffix.length == 0) "" else (if (prefix.endsWith(" ")) "" else " ")

        prefix + infix + suffix
      }
    }
  }
}

object HttpRequestLogger{
  import blueeyes.parsers.W3ExtendedLogGrammar._
  import blueeyes.parsers.W3ExtendedLogAST._
  
  // <date>  = 4<digit> "-" 2<digit> "-" 2<digit> YYYY-MM-DD
  private val DateFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")
  
  // <time>  = 2<digit> ":" 2<digit> [":" 2<digit> ["." *<digit>] HH-MM-SS
  private val TimeFormatter = DateTimeFormat.forPattern("HH:mm:ss.S")

  private val IpIdentifierValue = try {
    InetAddress.getLocalHost.getHostAddress
  }
  catch {
   case error: Throwable => "127.0.0.1"
  }

  private val DnsNameIdentifierValue = try {
    InetAddress.getLocalHost.getHostName
  }
  catch {
   case error: Throwable => "localhost"
  }

  private def lift[T, S](f: (HttpRequest[T], Future[HttpResponse[S]]) => Future[String]): HttpRequestLogger[T, S] = new HttpRequestLoggerImpl[T, S] (f)

  private[HttpRequestLogger] class HttpRequestLoggerImpl[T, S](f: (HttpRequest[T], Future[HttpResponse[S]]) => Future[String]) extends HttpRequestLogger[T, S]{
    def apply(request: HttpRequest[T], response: Future[HttpResponse[S]]): Future[String] = f(request, response)
  }
  
  /** Creates a logger from a W3 Extended Log fields directive. e.g.:
   *
   * #Fields: time cs-method cs-uri
   */
  def apply[T, S](fieldsDirective: FieldsDirective)(implicit clock: Clock, requestBijection: Bijection[T, ByteChunk], responseBijection: Bijection[S, ByteChunk]): HttpRequestLogger[T, S] = {
    def encodeBase64(chunk: Option[ByteChunk]) = chunk.map(AggregatedByteChunk(_).map(aggregated => new String(Base64.encodeBase64(aggregated.data), "UTF-8"))).getOrElse(Future.sync(""))
    def apply0(identifiers: List[FieldIdentifier]): HttpRequestLogger[T, S] = identifiers match {
      case Nil =>
        lift((rq, rs) => Future.sync(""))
      
      case head :: tail =>
        (lift { (request: HttpRequest[T], response: Future[HttpResponse[S]]) => 
          head match {
            case DateIdentifier =>
              Future.sync(DateFormatter.print(clock.now()))
          
            case TimeIdentifier =>
              Future.sync(TimeFormatter.print(clock.now()))
          
            case TimeTakenIdentifier =>
              val start = clock.now().getMillis
              
              response.map { _ =>
                val end = clock.now().getMillis
                
                val deltaSeconds = (end - start) / 1000.0
                
                deltaSeconds.toString
              }
          
            case BytesIdentifier =>
              import HttpHeaders._
              
              response.map { response =>
                val contentLength = (for (`Content-Length`(length) <- response.headers.raw) yield length).headOption.getOrElse(0L)
                
                contentLength.toString
              }
          
            case CachedIdentifier =>
              import HttpHeaders._
              
              response.map { response =>
                val cached = (for (Age(age) <- response.headers.raw) yield age).headOption.getOrElse(0.0) > 0
                
                if (cached) "1" else "0"
              }
          
            case IpIdentifier(prefix) => prefix match {
              case ClientPrefix => Future.sync(request.remoteHost.map(_.getHostAddress).getOrElse(""))
              case ServerPrefix => Future.sync(IpIdentifierValue)
              case _   => Future.sync("")
            }
            case DnsNameIdentifier(prefix) => prefix match {
              case ClientPrefix => Future.sync(request.remoteHost.map(_.getHostName).getOrElse(""))
              case ServerPrefix => Future.sync(DnsNameIdentifierValue)
              case _   => Future.sync("")
            }
            case ContentIdentifier(prefix) => prefix match {
              case ClientToServerPrefix => encodeBase64(request.content.map(requestBijection(_)))
              case ServerToClientPrefix => response flatMap { response => encodeBase64(response.content.map(responseBijection(_))) }
              case _   => Future.sync("")
            }
            case StatusIdentifier(prefix) => prefix match {
              case ServerToClientPrefix => response map { response => response.status.code.name }
              case _   => Future.sync("")
            }
            case CommentIdentifier(prefix) => prefix match {
              case ServerToClientPrefix => response map { response => response.status.reason }
              case _   => Future.sync("")
            }
            case MethodIdentifier(prefix) => prefix match {
              case ClientToServerPrefix => Future.sync(request.method.value)
              case _   => Future.sync("")
            }
            case UriIdentifier(prefix) => prefix match {
              case ClientToServerPrefix => Future.sync(request.uri.toString)
              case _   => Future.sync("")
            }
            case UriStemIdentifier(prefix) => prefix match {
              case ClientToServerPrefix => Future.sync(request.uri.path.getOrElse(""))
              case _   => Future.sync("")
            }
            case UriQueryIdentifier(prefix) => prefix match {
              case ClientToServerPrefix => Future.sync(request.uri.query.getOrElse(""))
              case _   => Future.sync("")
            }
            case HeaderIdentifier(prefix, header) =>
              def find(key: String, headers: Map[String, String]) = headers find {keyValue => keyValue._1.toLowerCase == key.toLowerCase} map {keyValue => keyValue._2} getOrElse ("")
              prefix match {
                case ClientToServerPrefix => Future.sync(find(header, request.headers.raw))
                case ServerToClientPrefix => response map { response => find(header, response.headers.raw) }
                case _   => Future.sync("")
              }
            case CustomIdentifier(value) =>
              Future.sync("")
          }
        }) :+ apply0(tail)
    }
    
    apply0(fieldsDirective.identifiers)
  }
}
