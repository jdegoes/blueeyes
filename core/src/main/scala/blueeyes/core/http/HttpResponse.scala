package blueeyes.core.http

import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.HttpVersions._

import scalaz._

sealed case class HttpResponse[+A](
  status: HttpStatus = HttpStatus(OK), 
  headers: HttpHeaders = HttpHeaders.Empty, 
  content: Option[A] = None, 
  version: HttpVersion = `HTTP/1.1`
) {
  def map[B](f: A => B): HttpResponse[B] = copy(content = content.map(f))
}

object HttpResponse {
  def empty[T] = HttpResponse[T](content = None)

  def error[T](th: Throwable)(implicit decode: String => T): HttpResponse[T] = th match {
    case e: HttpException => HttpResponse[T](HttpStatus(e.failure), headers = HttpHeaders(`Content-Type`(text/plain)), content = Some(decode(e.reason)))
    case e => HttpResponse[T](HttpStatus(HttpStatusCodes.InternalServerError))
  }

  implicit val instances: Functor[HttpResponse] = new Functor[HttpResponse] {
    def map[A, B](fa: HttpResponse[A])(f: A => B) = fa.map(f)
  }

  def modifyHeaders(f: HttpHeaders => HttpHeaders) = new NaturalTransformation[HttpResponse, HttpResponse] {
    def apply[A](r: HttpResponse[A]): HttpResponse[A] = r.copy(headers = f(r.headers))
  }
}

trait HttpResponseImplicits {
  implicit def any2ResponseOk[T](content: T) = new ResponseOk[T](content)
  class ResponseOk[+T](content: T) { self => 
    def ok[TT >: T]: HttpResponse[TT] = HttpResponse(content = Some(self.content))
  }
}
