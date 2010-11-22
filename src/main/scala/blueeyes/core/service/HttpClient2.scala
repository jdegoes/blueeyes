package blueeyes.core.service

import blueeyes.util.Future
import blueeyes.core.http._

trait HttpClient2 {
  def apply[T, S](builder: HttpResponseHandler[T, S]): Future[S] = {
    val initialRequest = HttpRequest[T](HttpMethods.GET, "")

    val (finalRequest, handler) = builder(initialRequest)

    doRequest(finalRequest).flatMap{
      response =>
        handler(response)
    }
  }

  protected def doRequest[T, S](request: HttpRequest[T]): Future[HttpResponse[T]]
}