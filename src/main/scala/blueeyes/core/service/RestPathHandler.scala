package blueeyes.core.service

import blueeyes.util.Future
import util.matching.Regex

case class RestPathHandler[T](pattern: PartialFunction[String, Map[Symbol, String]], 
                              handler: HttpRequest[T] => Future[HttpResponse[T]]) extends PartialFunction[String, HttpRequest[T] => Future[HttpResponse[T]]] {
 def isDefinedAt(url: String) = pattern.isDefinedAt(url)

 def apply(url: String) = (request: HttpRequest[T]) => {
   val symbols = pattern(url)

   handler(HttpRequest(request.method, request.uri, (symbols ++ request.parameters), request.headers, request.content, request.remoteHost, request.version))
 }
}

trait RestPathHandlerImplicits {
  implicit def fullHandler2PartialHandler[T](f: HttpRequest[T] => Future[HttpResponse[T]]): PartialFunction[HttpRequest[T], Future[HttpResponse[T]]] = {
    return new PartialFunction[HttpRequest[T], Future[HttpResponse[T]]] {
      def isDefinedAt(p: HttpRequest[T]) = true
      
      def apply(p: HttpRequest[T]) = f(p)
    }
  }
}
object RestPathHandlerImplicits extends RestPathHandlerImplicits
