package blueeyes.core

import blueeyes.util.Future
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.core.service.{RestPathPattern}

package object service {
  type HttpRequestHandler[T] = PartialFunction[HttpRequest[T], Future[HttpResponse[T]]]

/*  
  def path[T, S](path: RestPathPattern)(hs: HttpRequestHandler[T, S]): HttpRequestHandler[T, S] = new HttpRequestHandler[T, S] {
    def isDefinedAt(r: HttpRequest[T]) = {
      
    }
  }
  def get[T, S](h: HttpRequestHandler[T, S]): HttpRequestHandler[T, S] {

  }*/
  
  implicit def fullHttpRequestHandler2PartialHttpRequestHandler[T](full: HttpRequest[T] => Future[HttpResponse[T]]) = new HttpRequestHandler[T] {
    def isDefinedAt(request: HttpRequest[T]) = true
    
    def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = full.apply(request)
  }
}