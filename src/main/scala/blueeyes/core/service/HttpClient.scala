package blueeyes.core.service

import blueeyes.util.Future
import blueeyes.core.http._

trait HttpClient[T]{
  def apply(request: HttpRequest[T]): Future[HttpResponse[T]]

  def exec[R](f: HttpClient[T] => Future[R]) = f(this)
}