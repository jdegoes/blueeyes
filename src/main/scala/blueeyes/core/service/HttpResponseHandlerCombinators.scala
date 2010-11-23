package blueeyes.core.service

import blueeyes.util.Future
import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import java.net.InetAddress
import blueeyes.core.data.Bijection

trait HttpResponseHandlerCombinators {
  def protocol[T, S](protocol: String)(h: HttpResponseHandler[T, S]): HttpResponseHandler[T, S] = path(protocol + "://")(h)

  def host[T, S](host: String)(h: HttpResponseHandler[T, S]): HttpResponseHandler[T, S]         = path(host)(h)

  def port[T, S](port: Int)(h: HttpResponseHandler[T, S]): HttpResponseHandler[T, S]            = path(":" + port.toString)(h)

  def path[T, S](path: String)(h: HttpResponseHandler[T, S]): HttpResponseHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(uri = request.uri + path))(h)

  def parameters[T, S](parameters: (Symbol, String)*)(h: HttpResponseHandler[T, S]): HttpResponseHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(parameters = Map[Symbol, String](parameters: _*)))(h)

  def headers[T, S](headers: (String, String)*)(h: HttpResponseHandler[T, S]): HttpResponseHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(headers = Map[String, String](headers: _*)))(h)

  def remoteHost[T, S](remoteHost: InetAddress)(h: HttpResponseHandler[T, S]): HttpResponseHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(headers = request.headers + Tuple2("X-Forwarded-For", remoteHost.getHostAddress())))(h)

  def version[T, S](version: HttpVersion)(h: HttpResponseHandler[T, S]): HttpResponseHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(version = version))(h)

  def contentType[T, S, R](mimeType: MimeType)(h: HttpResponseHandler[R, S])(implicit b: Bijection[T, R]): HttpResponseHandler[T, S] = {    
    (initialRequest: HttpRequest[T]) => {
      val (userRequest, handler) = h(initialRequest.copy(content = initialRequest.content.map(b.apply), headers = initialRequest.headers + Tuple2("Content-Type", mimeType.value)))
      val finalRequest           = userRequest.copy(content = userRequest.content.map(b.unapply))

      (finalRequest, (response: HttpResponse[T]) => {
        val finalResponse = response.copy(content = response.content.map(b.apply))
        handler(finalResponse)
      })
    }
  }

  private def apply[T, S](copy: (HttpRequest[T]) => HttpRequest[T])(h: HttpResponseHandler[T, S]) = {
    (request: HttpRequest[T]) => (
      h(copy(request))
    )
  }

  def get[T, S]()(h: HttpResponse[T] => Future[S]): HttpResponseHandler[T, S] = {
    (request: HttpRequest[T]) => (request.copy(method = HttpMethods.GET), h)
  }
  def put[T, S](content: T)(h: HttpResponse[T] => Future[S]): HttpResponseHandler[T, S] = {
    (request: HttpRequest[T]) => (request.copy(method = HttpMethods.PUT, content = Some(content)), h)
  }
  def post[T, S](content: T)(h: HttpResponse[T] => Future[S]): HttpResponseHandler[T, S] = {
    (request: HttpRequest[T]) => (request.copy(method = HttpMethods.POST, content = Some(content)), h)
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