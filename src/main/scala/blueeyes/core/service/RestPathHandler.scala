package blueeyes.core.service

import blueeyes.util.Future
import util.matching.Regex

case class RestPathHandler[T](pattern: PartialFunction[String, Map[Symbol, String]], 
                              handler: (Map[Symbol, String], HttpRequest[T]) => Future[HttpResponse[T]]) extends PartialFunction[String, HttpRequest[T] => Future[HttpResponse[T]]] {
 def isDefinedAt(url: String) = pattern.isDefinedAt(url)

 def apply(url: String) = (request: HttpRequest[T]) => {
   val symbols = pattern(url)

   handler(symbols, request)
 }
}

trait RestPathHandlerImplicits {
  implicit def fullHandler2PartialHandler[T](f: (Map[Symbol, String], HttpRequest[T]) => Future[HttpResponse[T]]): PartialFunction[(Map[Symbol, String], HttpRequest[T]), Future[HttpResponse[T]]] = {
    return new PartialFunction[(Map[Symbol, String], HttpRequest[T]), Future[HttpResponse[T]]] {
      def isDefinedAt(p: (Map[Symbol, String], HttpRequest[T])) = true
      
      def apply(p: (Map[Symbol, String], HttpRequest[T])) = f(p)
    }
  }
}
object RestPathHandlerImplicits extends RestPathHandlerImplicits
