package blueeyes.core.service

import blueeyes.util.Future
import blueeyes.core.http._
import blueeyes.core.data.Bijection
import java.net.InetAddress

trait HttpResponseHandlerCombinators{
  def protocol[T, S](protocol: String)(handler: HttpClientHandler[T, S]): HttpClientHandler[T, S] =
    path(protocol + "://")(handler)

  def host[T, S](host: String)(h: HttpClientHandler[T, S]): HttpClientHandler[T, S] = path(host)(h)

  def port[T, S](port: Int)(h: HttpClientHandler[T, S]): HttpClientHandler[T, S]    = path(":" + port.toString)(h)

  def path[T, S](path: String)(handler: HttpClientHandler[T, S]): HttpClientHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(uri = path + request.uri))(handler)

  def parameters[T, S](parameters: (Symbol, String)*)(h: HttpClientHandler[T, S]): HttpClientHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(parameters = Map[Symbol, String](parameters: _*)))(h)

  def headers[T, S](headers: (String, String)*)(h: HttpClientHandler[T, S]): HttpClientHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(headers = Map[String, String](headers: _*)))(h)

  def remoteHost[T, S](remoteHost: InetAddress)(h: HttpClientHandler[T, S]): HttpClientHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(headers = request.headers + Tuple2("X-Forwarded-For", remoteHost.getHostAddress())))(h)

  def version[T, S](version: HttpVersion)(h: HttpClientHandler[T, S]): HttpClientHandler[T, S] =
    apply((request: HttpRequest[T]) => request.copy(version = version))(h)

  def contentType[T, S, R](mimeType: MimeType)(handler: HttpClientHandler[T, R])(implicit b: Bijection[T, S]): HttpClientHandler[S, R] = {
    (client: HttpClient[S]) => {
      val wrappedClient = new HttpClient[T] {
        def apply(initialRequest: HttpRequest[T]): Future[HttpResponse[T]] = {
          val response: Future[HttpResponse[S]] = client.apply(initialRequest.copy(content = initialRequest.content.map(b.apply), headers = initialRequest.headers + Tuple2("Content-Type", mimeType.value)))

          response.map(value => value.copy(content = value.content.map(b.unapply)))
        }
      }
      handler(wrappedClient)
    }
  }

  def get[T, S](handler: HttpResponse[T] => Future[S]): HttpClientHandler[T, S] = method[T, S](HttpMethods.GET, handler)

  def put[T, S](content: T)(handler: HttpResponse[T] => Future[S]): HttpClientHandler[T, S] = method[T, S](HttpMethods.PUT, handler, Some(content))

  def post[T, S](content: T)(handler: HttpResponse[T] => Future[S]): HttpClientHandler[T, S] = method[T, S](HttpMethods.POST, handler, Some(content))

  def delete[T, S]()(handler: HttpResponse[T] => Future[S]): HttpClientHandler[T, S] = method[T, S](HttpMethods.DELETE, handler)

  def options[T, S]()(handler: HttpResponse[T] => Future[S]): HttpClientHandler[T, S] = method[T, S](HttpMethods.OPTIONS, handler)

  def head[T, S]()(handler: HttpResponse[T] => Future[S]): HttpClientHandler[T, S] = method[T, S](HttpMethods.HEAD, handler)

  def connect[T, S]()(handler: HttpResponse[T] => Future[S]): HttpClientHandler[T, S] = method[T, S](HttpMethods.CONNECT, handler)

  def trace[T, S]()(handler: HttpResponse[T] => Future[S]): HttpClientHandler[T, S] = method[T, S](HttpMethods.TRACE, handler)

  def custom[T, S](custom: String)(handler: HttpResponse[T] => Future[S]): HttpClientHandler[T, S] =
    method[T, S](HttpMethods.CUSTOM(custom), handler)

  def secure[T, S](handler: HttpClientHandler[T, S]): HttpClientHandler[T, S] = {
    (client: HttpClient[T]) => {
      val wrappedClient: HttpClient[T] = new HttpClient[T] {
        def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = client(request)
        override def isDefinedAt(request: HttpRequest[T]) = request.scheme == "https"
      }
      handler(wrappedClient)
    }
  }
  
  private def method[T, S](method: HttpMethod, handler: HttpResponse[T] => Future[S], content: Option[T] = None) = {
    (client: HttpClient[T]) => {
      client.apply(HttpRequest[T](method = method, content = content, uri = "")).flatMap(handler)
    }
  }

  private def apply[T, S](copy: (HttpRequest[T]) => HttpRequest[T])(handler: HttpClientHandler[T, S]): HttpClientHandler[T, S] = {
    (client: HttpClient[T]) => {
      val wrappedClient = new HttpClient[T] {
        def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = client.apply(copy(request))
      }

      handler(wrappedClient)
    }
  }
}