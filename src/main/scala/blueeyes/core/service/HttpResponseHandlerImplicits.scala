package blueeyes.core.service

trait HttpResponseHandlerImplicits{
//  implicit def httpResponseHandlerCompositionSugar[T, S](h1: HttpResponseHandler[T, S]) = new {
//    def ~ [R](p2: HttpResponseHandler[T, R]): HttpResponseHandler[T, (S, R)] = {
//
//    }
//  }
}
object HttpResponseHandlerImplicits extends HttpResponseHandlerImplicits
