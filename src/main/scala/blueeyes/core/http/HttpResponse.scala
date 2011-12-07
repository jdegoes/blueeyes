package blueeyes.core.http

import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.HttpVersions._
import com.weiglewilczek.slf4s.Logging

sealed case class HttpResponse[+T](
  status: HttpStatus = HttpStatus(OK), 
  headers: HttpHeaders = HttpHeaders.Empty, 
  content: Option[T] = None, 
  version: HttpVersion = `HTTP/1.1`
) extends Logging {
  if (status.code != HttpStatusCodes.OK) {
    logger.trace("Response constructed with non-OK " + status + " at: \n" + Thread.currentThread.getStackTrace.mkString("\t", "\t\n", "\n"))
  }
}

object HttpResponse {
  def empty[T] = HttpResponse[T](content = None)
}

trait HttpResponseImplicits {
  implicit def any2ResponseOk[T](content: T) = new ResponseOk[T](content)
  class ResponseOk[+T](content: T) { self => 
    def ok[TT >: T]: HttpResponse[TT] = HttpResponse(content = Some(self.content))
  }
}
