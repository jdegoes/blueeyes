package blueeyes.core.service

import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.joda.time.format.DateTimeFormat
import org.joda.time.format.DateTimeFormatter

import blueeyes.core.http.{HttpRequest, HttpResponse, HttpHeaders}
import blueeyes.util.Future
import blueeyes.util.Clock

/** A request logger is a function from (request/future of response) to future 
 * of log line. Request loggers do not have side effects.
 */
trait HttpRequestLogger[T, S] extends ((HttpRequest[T], Future[HttpResponse[S]]) => Future[String]) { self =>
  /** Combines this logger with the specified logger to produce another logger.
   * If necessary, the items are separated by a single space character.
   */
  def :+ (logger: HttpRequestLogger[T, S]): HttpRequestLogger[T, S] = new HttpRequestLogger[T, S] {
    def apply(request: HttpRequest[T], response: Future[HttpResponse[S]]): Future[String] = {
      self(request, response).zip(logger(request, response)).map { t =>
        val (prefix, suffix) = t
        
        val infix = if (suffix.length == 0) "" else (if (prefix.endsWith(" ")) "" else " ")

        prefix + infix + suffix
      }
    }
  }
}

object HttpRequestLogger {
  import blueeyes.parsers.W3ExtendedLogGrammar._
  import blueeyes.parsers.W3ExtendedLogAST._
  
  // <date>  = 4<digit> "-" 2<digit> "-" 2<digit> YYYY-MM-DD
  private val DateFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")
  
  // <time>  = 2<digit> ":" 2<digit> [":" 2<digit> ["." *<digit>] HH-MM-SS
  private val TimeFormatter = DateTimeFormat.forPattern("HH:mm:ss.S")
  
  private def lift[T, S](f: (HttpRequest[T], Future[HttpResponse[S]]) => Future[String]): HttpRequestLogger[T, S] = new HttpRequestLogger[T, S] {
    def apply(request: HttpRequest[T], response: Future[HttpResponse[S]]): Future[String] = f(request, response)
  }
  
  /** Creates a logger from a W3 Extended Log fields directive. e.g.:
   *
   * #Fields: time cs-method cs-uri
   */
  def apply[T, S](fieldsDirective: FieldsDirective)(implicit clock: Clock): HttpRequestLogger[T, S] = {
    def apply0(identifiers: List[FieldIdentifier]): HttpRequestLogger[T, S] = identifiers match {
      case Nil =>
        lift((rq, rs) => Future.lift(""))
      
      case head :: tail =>
        (lift { (request: HttpRequest[T], response: Future[HttpResponse[S]]) => 
          head match {
            case DateIdentifier =>
              Future.lift(DateFormatter.print(clock.now()))
          
            case TimeIdentifier =>
              Future.lift(TimeFormatter.print(clock.now()))
          
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
                val contentLength = (for (`Content-Length`(length) <- response.headers) yield length).headOption.getOrElse(0L)
                
                contentLength.toString
              }
          
            case CachedIdentifier =>
              import HttpHeaders._
              
              response.map { response =>
                val cached = (for (Age(age) <- response.headers) yield age).headOption.getOrElse(0.0) > 0
                
                if (cached) "1" else "0"
              }
          
            case IpIdentifier(prefix) =>
              // TODO
              Future.lift("")
          
            case DnsNameIdentifier(prefix) =>
              // TODO
              Future.lift("")
          
            case StatusIdentifier(prefix) =>
              // TODO
              Future.lift("")
          
            case CommentIdentifier(prefix) =>
              // TODO
              Future.lift("")
          
            case MethodIdentifier(prefix) =>
              // TODO
              Future.lift("")
          
            case UriIdentifier(prefix) =>
              // TODO
              Future.lift("")
          
            case UriStemIdentifier(prefix) =>
              // TODO
              Future.lift("")
          
            case UriQueryIdentifier(prefix) =>
              // TODO
              Future.lift("")
         
            case HeaderIdentifier(prefix, header) =>
              // TODO
              Future.lift("")
          
            case CustomIdentifier(value) =>
              Future.lift("")
          }
        }) :+ apply0(tail)
    }
    
    apply0(fieldsDirective.identifiers)
  }
}