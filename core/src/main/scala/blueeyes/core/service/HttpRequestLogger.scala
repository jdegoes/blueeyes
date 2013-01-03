package blueeyes.core.service

import blueeyes.bkka._
import blueeyes.core.data._
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpHeaders}
import blueeyes.parsers.W3ExtendedLogAST.FieldIdentifier
import blueeyes.util.Clock

import akka.dispatch.Future
import akka.dispatch.Promise

import java.net.InetAddress
import org.joda.time.format.DateTimeFormat

/** A request logger is a function from (request/future of response) to future
 * of log line. Request loggers do not have side effects.
 */
trait HttpRequestLogger[T, S] extends ((HttpRequest[T], Future[HttpResponse[S]]) => Future[List[(FieldIdentifier, Either[String, Array[Byte]])]]) { self =>
  /** Combines this logger with the specified logger to produce another logger.
   * If necessary, the items are separated by a single space character.
   */
  def :+ (logger: HttpRequestLogger[T, S]): HttpRequestLogger[T, S] = new CompositeHttpRequestLogger(logger)

  private[HttpRequestLogger] class CompositeHttpRequestLogger(logger: HttpRequestLogger[T, S]) extends HttpRequestLogger[T, S]{
    def apply(request: HttpRequest[T], response: Future[HttpResponse[S]]): Future[List[(FieldIdentifier, Either[String, Array[Byte]])]] = {
     (self(request, response) zip logger(request, response)).map { 
        case (prefix, suffix) => prefix ::: suffix
      }
    }
  }
}

//TODO: get rid of AkkaDefaults
object HttpRequestLogger extends AkkaDefaults {
  import blueeyes.parsers.W3ExtendedLogGrammar._
  import blueeyes.parsers.W3ExtendedLogAST._
  
  // <date>  = 4<digit> "-" 2<digit> "-" 2<digit> YYYY-MM-DD
  private val DateFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")
  
  // <time>  = 2<digit> ":" 2<digit> [":" 2<digit> ["." *<digit>] HH-MM-SS
  private val TimeFormatter = DateTimeFormat.forPattern("HH:mm:ss.S")

  private val IpIdentifierValue = try {
      InetAddress.getLocalHost.getHostAddress
    } catch {
      case error: Throwable => "127.0.0.1"
  }

  private val DnsNameIdentifierValue = try {
      InetAddress.getLocalHost.getHostName
    } catch {
      case error: Throwable => "localhost"
  }

  
  /** Creates a logger from a W3 Extended Log fields directive. e.g.:
   *
   * #Fields: time cs-method cs-uri
   */
  def apply[T, S](fieldsDirective: FieldsDirective)(implicit clock: Clock, req2c: T => ByteChunk, resp2c: S => ByteChunk): HttpRequestLogger[T, S] = new HttpRequestLogger[T, S] {
    def apply(request: HttpRequest[T], response: Future[HttpResponse[S]]): Future[List[(FieldIdentifier, Either[String, Array[Byte]])]] = {
      Future.sequence(fieldsDirective.identifiers.map(log(_, request, response)))
    }
  }

  private def log[T, S](fieldIdentifier: FieldIdentifier, request: HttpRequest[T], response: Future[HttpResponse[S]])(implicit clock: Clock, req2c: T => ByteChunk, resp2c: S => ByteChunk): Future[(FieldIdentifier, Either[String, Array[Byte]])] = {
    def aggregate(chunk: Option[ByteChunk]): Future[Either[String, Array[Byte]]] = {
      chunk map { c =>
        DefaultBijections.futureByteArrayToChunk.unapply(c) map { Right(_) }
      } getOrElse {
        Future(Right(Array[Byte]()))
      }
    }

    val value: Future[Either[String, Array[Byte]]] = fieldIdentifier match {
      case DateIdentifier =>
        Promise.successful(Left(DateFormatter.print(clock.now())))

      case TimeIdentifier =>
        Promise.successful(Left(TimeFormatter.print(clock.now())))

      case TimeTakenIdentifier =>
        val start = clock.now().getMillis

        response.map[Either[String, Array[Byte]]] { _ =>
          val deltaSeconds = (clock.now().getMillis - start) / 1000.0
          Left(deltaSeconds.toString)
        } 

      case BytesIdentifier =>
        import HttpHeaders._

        response.map[Either[String, Array[Byte]]] { response =>
          val contentLength = (for (`Content-Length`(length) <- response.headers.raw) yield length).headOption.getOrElse(0L)
          Left(contentLength.toString)
        } recover {
          case ex => Left(ex.getMessage)
        }

      case CachedIdentifier =>
        import HttpHeaders._

        response.map[Either[String, Array[Byte]]] { response =>
          val cached = (for (Age(age) <- response.headers.raw) yield age).headOption.getOrElse(0.0) > 0
          Left(if (cached) "1" else "0")
        } recover {
          case ex => Left(ex.getMessage)
        }

      case IpIdentifier(prefix) => prefix match {
        case ClientPrefix => Promise.successful(Left(request.remoteHost.map(_.getHostAddress).getOrElse("")))
        case ServerPrefix => Promise.successful(Left(IpIdentifierValue))
        case _   => Promise.successful(Left(""))
      }
      case DnsNameIdentifier(prefix) => prefix match {
        case ClientPrefix => Promise.successful(Left(request.remoteHost.map(_.getHostName).getOrElse("")))
        case ServerPrefix => Promise.successful(Left(DnsNameIdentifierValue))
        case _   => Promise.successful(Left(""))
      }
      case ContentIdentifier(prefix) => prefix match {
        case ClientToServerPrefix => aggregate(request.content.map(req2c(_)))
        case ServerToClientPrefix => 
          response.flatMap[Either[String, Array[Byte]]] { 
            response => aggregate(response.content.map(resp2c(_))) 
          } recover { 
            case ex => Left(ex.getMessage)
          }

        case _   => Promise.successful(Left(""))
      }
      case StatusIdentifier(prefix) => prefix match {
        case ServerToClientPrefix => 
          response.map[Either[String, Array[Byte]]] { 
            response => Left(response.status.code.name) 
          } recover { 
            case ex => Left(ex.getMessage) 
          }

        case _   => Promise.successful(Left(""))
      }
      case CommentIdentifier(prefix) => prefix match {
        case ServerToClientPrefix => 
          response.map[Either[String, Array[Byte]]] { 
            response => Left(response.status.reason) 
          } recover { 
            case ex => Left(ex.getMessage) 
          }

        case _   => Promise.successful(Left(""))
      }
      case MethodIdentifier(prefix) => prefix match {
        case ClientToServerPrefix => Promise.successful(Left(request.method.value))
        case _   => Promise.successful(Left(""))
      }
      case UriIdentifier(prefix) => prefix match {
        case ClientToServerPrefix => Promise.successful(Left(request.uri.toString))
        case _   => Promise.successful(Left(""))
      }
      case UriStemIdentifier(prefix) => prefix match {
        case ClientToServerPrefix => Promise.successful(Left(request.uri.path.getOrElse("")))
        case _   => Promise.successful(Left(""))
      }
      case UriQueryIdentifier(prefix) => prefix match {
        case ClientToServerPrefix => Promise.successful(Left(request.uri.query.getOrElse("")))
        case _   => Promise.successful(Left(""))
      }
      case HeaderIdentifier(prefix, header) =>
        def find(key: String, headers: Map[String, String]) = headers find {keyValue => keyValue._1.toLowerCase == key.toLowerCase} map {keyValue => keyValue._2} getOrElse ("")
        prefix match {
          case ClientToServerPrefix => Promise.successful(Left(find(header, request.headers.raw)))
          case ServerToClientPrefix => 
            response.map[Either[String, Array[Byte]]] { 
              response => Left(find(header, response.headers.raw)) 
            } recover { 
              case ex => Left(ex.getMessage) 
            }

          case _   => Promise.successful(Left(""))
        }
      case CustomIdentifier(value) =>
        Promise.successful(Left(""))
    }

    value.map((fieldIdentifier, _))
  }

}
