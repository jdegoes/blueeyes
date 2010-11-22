package blueeyes.core.service

import blueeyes.util.Future
import blueeyes.core.http._
import java.net.InetAddress

trait HttpResponseHandlerCombinators {
  def path[T, S](path: String)(h: HttpResponseHandler[T, S]): HttpResponseHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(uri = request.uri + path))(h)

  def parameters[T, S](parameters: Map[Symbol, String])(h: HttpResponseHandler[T, S]): HttpResponseHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(parameters = parameters))(h)

  def headers[T, S](headers: Map[String, String])(h: HttpResponseHandler[T, S]): HttpResponseHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(headers = headers))(h)

  def content[T, S](content: T)(h: HttpResponseHandler[T, S]): HttpResponseHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(content = Some(content)))(h)

  def remoteHost[T, S](remoteHost: InetAddress)(h: HttpResponseHandler[T, S]): HttpResponseHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(headers = request.headers + Tuple2("X-Forwarded-For", remoteHost.getHostAddress())))(h)

  def version[T, S](version: HttpVersion)(h: HttpResponseHandler[T, S]): HttpResponseHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(version = version))(h)

  def contentType[T, S](mimeType: MimeType)(h: HttpResponseHandler[T, S]): HttpResponseHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(headers = request.headers + Tuple2("content-type", mimeType.value)))(h)

  private def apply[T, S](copy: (HttpRequest[T]) => HttpRequest[T])(h: HttpResponseHandler[T, S]) = {
    (request: HttpRequest[T]) => (
      h(copy(request))
    )
  }

  def get[T, S]()(h: HttpResponse[T] => Future[S]): HttpResponseHandler[T, S] = {
    (request: HttpRequest[T]) => (request.copy(method = HttpMethods.GET), h)
  }
  def put[T, S]()(h: HttpResponse[T] => Future[S]): HttpResponseHandler[T, S] = {
    (request: HttpRequest[T]) => (request.copy(method = HttpMethods.PUT), h)
  }
  def post[T, S]()(h: HttpResponse[T] => Future[S]): HttpResponseHandler[T, S] = {
    (request: HttpRequest[T]) => (request.copy(method = HttpMethods.POST), h)
  }
  def delete[T, S]()(h: HttpResponse[T] => Future[S]): HttpResponseHandler[T, S] = {
    (request: HttpRequest[T]) => (request.copy(method = HttpMethods.DELETE), h)
  }
  def options[T, S]()(h: HttpResponse[T] => Future[S]): HttpResponseHandler[T, S] = {
    (request: HttpRequest[T]) => (request.copy(method = HttpMethods.OPTIONS), h)
  }
  def head[T, S]()(h: HttpResponse[T] => Future[S]): HttpResponseHandler[T, S] = {
    (request: HttpRequest[T]) => (request.copy(method = HttpMethods.HEAD), h)
  }
  def connect[T, S]()(h: HttpResponse[T] => Future[S]): HttpResponseHandler[T, S] = {
    (request: HttpRequest[T]) => (request.copy(method = HttpMethods.CONNECT), h)
  }
  def trace[T, S]()(h: HttpResponse[T] => Future[S]): HttpResponseHandler[T, S] = {
    (request: HttpRequest[T]) => (request.copy(method = HttpMethods.TRACE), h)
  }
  def custom[T, S](method: String)(h: HttpResponse[T] => Future[S]): HttpResponseHandler[T, S] = {
    (request: HttpRequest[T]) => (request.copy(method = HttpMethods.CUSTOM(method)), h)
  }
}