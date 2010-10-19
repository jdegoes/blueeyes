package blueeyes.core.service

import blueeyes.util.Future

trait HttpClient[T] {
  def apply(request: HttpRequest[T]): Future[HttpResponse[T]]
}

