package blueeyes.core

import blueeyes.util.Future
import blueeyes.core.http._

package object service extends HttpRequestHandlerCombinators with RestPathPatternImplicits {
  type HttpRequestHandler[T] = PartialFunction[HttpRequest[T], Future[HttpResponse[T]]]
  type HttpRequestHandler2[T, S] = PartialFunction[HttpRequest[T], Future[HttpResponse[S]]]
  
  implicit def fullHttpRequestHandler2PartialHttpRequestHandler[T](full: HttpRequest[T] => Future[HttpResponse[T]]) = new HttpRequestHandler[T] {
    def isDefinedAt(request: HttpRequest[T]) = true
    
    def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = full.apply(request)
  }
  
  implicit def partialFunctionCompositionSugar[A, B](p1: PartialFunction[A, B]) = new {
    def ~ (p2: PartialFunction[A, B]): PartialFunction[A, B] = p1.orElse(p2)
  }
}