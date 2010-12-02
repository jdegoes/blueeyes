package blueeyes.core.service

import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.util.Future
import blueeyes.core.data._

trait HttpRequestHandlerImplicits {
  implicit def partialFunctionCompositionSugar[A, B](p1: PartialFunction[A, B]) = new {
    def ~ (p2: PartialFunction[A, B]): PartialFunction[A, B] = p1.orElse(p2)
  }
  
  implicit def orCompositionSugar[T, S](r1: HttpRequestHandler2[T, S] => HttpRequestHandler2[T, S]) = new {
    def | (r2: HttpRequestHandler2[T, S] => HttpRequestHandler2[T, S])(h: HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = r1(h).orElse(r2(h))
    
    def || (r2: HttpRequestHandler2[T, S] => HttpRequestHandler2[T, S])(h: HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = r1(h).orElse(r2(h))
  }
  
  implicit def andCompositionSugar[T, S](r1: HttpRequestHandler2[T, S] => HttpRequestHandler2[T, S]) = new {
    def & (r2: HttpRequestHandler2[T, S] => HttpRequestHandler2[T, S])(h: HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = r1.andThen(r2)(h)
    
    def && (r2: HttpRequestHandler2[T, S] => HttpRequestHandler2[T, S])(h: HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = r1.andThen(r2)(h)
  }
}
object HttpRequestHandlerImplicits extends HttpRequestHandlerImplicits