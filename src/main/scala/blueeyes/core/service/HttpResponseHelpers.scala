package blueeyes.core.service

import blueeyes.util.Future
import blueeyes.util.RichThrowableImplicits._
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.HttpVersions._

trait HttpResponseHelpers {
  /** Shorthand function to create a future of an HttpResponse from the given parameters.
   */
  def respond[T](status: HttpStatus = HttpStatus(OK), headers: Map[String, String] = Map(), content: Option[T] = None): Future[HttpResponse[T]] = {
    Future(HttpResponse[T](status, headers, content))
  }
  
  /** Shorthand function to create a simple response based on a future and 
   * headers. If the future is delivered, the OK status code will be returned,
   * but if the future is canceled, an InternalServerError status code will
   * be returned.
   */
  def respondLater[T](content: Future[T], headers: Map[String, String] = Map()): Future[HttpResponse[T]] = {
    val f = new Future[HttpResponse[T]]
    
    content.map { content =>
      f.deliver(HttpResponse[T](HttpStatus(OK), headers, Some(content)))
    }.ifCanceled { why =>
      f.deliver { 
        why match {
          case Some(error) => HttpResponse[T](status = HttpStatus(InternalServerError, error.fullStackTrace))
        
          case None => HttpResponse[T](status = HttpStatus(InternalServerError, "The response was unexpectedly canceled on the server-side"))
        }
      }
    }
    
    f
  }
}
object HttpResponseHelpers extends HttpResponseHelpers