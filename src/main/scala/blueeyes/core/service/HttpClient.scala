package blueeyes.core.service

trait HttpClient[T] {
  def apply(request: HttpRequest[T]): Future[HttpResponse[T]]
}