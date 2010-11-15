package blueeyes.core.service

import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.util.Future
import blueeyes.core.data._

trait HttpRequestHandlerImplicits {
  implicit def fullHttpRequestHandler2PartialHttpRequestHandler[T](full: HttpRequest[T] => Future[HttpResponse[T]]) = new HttpRequestHandler[T] {
    def isDefinedAt(request: HttpRequest[T]) = true
  
    def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = full.apply(request)
  }
  
  /*
  // Not useful due to strange behavior of Scala implicits on (anonymous) functions
  implicit def doublyTypedRequestHandlerToSinglyTypedRequestHandler[S, R, T]
    (h: HttpRequest[S] => Future[HttpResponse[R]])(implicit in: DataTranscoder[T, S], out: DataTranscoder[R, T]): HttpRequest[T] => Future[HttpResponse[T]] = {
    (request: HttpRequest[T]) => {
      h(request.copy(content = request.content.map(in.transcode))).map { response =>
        response.copy(content = response.content.map(out.transcode))
      }
    }
  }
  
  implicit def singlyTypedRequestHandlerToSinglyTypedRequestHandler[T, S]
    (h: HttpRequest[T] => Future[HttpResponse[T]])(implicit transcoder: DataTranscoder[T, S]): HttpRequest[S] => Future[HttpResponse[S]] = {
    (request: HttpRequest[S]) => {
      h(request.copy(content = request.content.map(transcoder.transcode.unapply))).map { response =>
        response.copy(content = response.content.map(transcoder.transcode.apply))
      }
    }
  }*/
  
  implicit def partialFunctionCompositionSugar[A, B](p1: PartialFunction[A, B]) = new {
    def ~ (p2: PartialFunction[A, B]): PartialFunction[A, B] = p1.orElse(p2)
  }
}
object HttpRequestHandlerImplicits extends HttpRequestHandlerImplicits