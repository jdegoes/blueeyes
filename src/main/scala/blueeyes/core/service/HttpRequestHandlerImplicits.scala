package blueeyes.core.service

import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.util.Future

trait HttpRequestHandlerImplicits {
  implicit def fullHttpRequestHandler2PartialHttpRequestHandler[T](full: HttpRequest[T] => Future[HttpResponse[T]]) = new HttpRequestHandler[T] {
    def isDefinedAt(request: HttpRequest[T]) = true
  
    def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = full.apply(request)
  }
  
  implicit def partialFunctionCompositionSugar[A, B](p1: PartialFunction[A, B]) = new {
    def ~ (p2: PartialFunction[A, B]): PartialFunction[A, B] = p1.orElse(p2)
  }
}
object HttpRequestHandlerImplicits extends HttpRequestHandlerImplicits