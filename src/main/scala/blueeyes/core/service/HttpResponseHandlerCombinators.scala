package blueeyes.core.service

import blueeyes.util.Future
import blueeyes.core.http._
import blueeyes.core.data.Bijection
import java.net.InetAddress

trait HttpResponseHandlerCombinators{
  def protocol[T, S](protocol: String)(transformer: HttpClientTransformer[T, S]): HttpClientTransformer[T, S] =
    path(protocol + "://")(transformer)

  def host[T, S](host: String)(transformer: HttpClientTransformer[T, S]): HttpClientTransformer[T, S] = path(host)(transformer)

  def port[T, S](port: Int)(transformer: HttpClientTransformer[T, S]): HttpClientTransformer[T, S]    = path(":" + port.toString)(transformer)

  def path[T, S](path: String)(transformer: HttpClientTransformer[T, S]): HttpClientTransformer[T, S] =
    apply((request: HttpRequest[T]) => request.copy(uri = path + request.uri))(transformer)

  def parameters[T, S](parameters: (Symbol, String)*)(transformer: HttpClientTransformer[T, S]): HttpClientTransformer[T, S] =
    apply((request: HttpRequest[T]) => request.copy(parameters = Map[Symbol, String](parameters: _*)))(transformer)

  def headers[T, S](headers: (String, String)*)(transformer: HttpClientTransformer[T, S]): HttpClientTransformer[T, S] =
    apply((request: HttpRequest[T]) => request.copy(headers = Map[String, String](headers: _*)))(transformer)

  def remoteHost[T, S](remoteHost: InetAddress)(transformer: HttpClientTransformer[T, S]): HttpClientTransformer[T, S] =
    apply((request: HttpRequest[T]) => request.copy(headers = request.headers + Tuple2("X-Forwarded-For", remoteHost.getHostAddress())))(transformer)

  def version[T, S](version: HttpVersion)(transformer: HttpClientTransformer[T, S]): HttpClientTransformer[T, S] =
    apply((request: HttpRequest[T]) => request.copy(version = version))(transformer)

  def contentType[T, S, R](mimeType: MimeType)(transformer: HttpClientTransformer[T, R])(implicit b: Bijection[T, S]): HttpClientTransformer[S, R] = {
    (client: HttpClient[S]) => {
      val wrappedClient = new HttpClient[T] {
        def apply(initialRequest: HttpRequest[T]): Future[HttpResponse[T]] = {
          val response: Future[HttpResponse[S]] = client.apply(initialRequest.copy(content = initialRequest.content.map(b.apply), headers = initialRequest.headers + Tuple2("Content-Type", mimeType.value)))

          response.map(value => value.copy(content = value.content.map(b.unapply)))
        }
      }
      transformer(wrappedClient)
    }
  }

  def get[T, S](transformer: HttpResponse[T] => Future[S]): HttpClientTransformer[T, S] = method[T, S](HttpMethods.GET, transformer)

  def put[T, S](content: T)(transformer: HttpResponse[T] => Future[S]): HttpClientTransformer[T, S] = method[T, S](HttpMethods.PUT, transformer, Some(content))

  def post[T, S](content: T)(transformer: HttpResponse[T] => Future[S]): HttpClientTransformer[T, S] = method[T, S](HttpMethods.POST, transformer, Some(content))

  def delete[T, S]()(transformer: HttpResponse[T] => Future[S]): HttpClientTransformer[T, S] = method[T, S](HttpMethods.DELETE, transformer)

  def options[T, S]()(transformer: HttpResponse[T] => Future[S]): HttpClientTransformer[T, S] = method[T, S](HttpMethods.OPTIONS, transformer)

  def head[T, S]()(transformer: HttpResponse[T] => Future[S]): HttpClientTransformer[T, S] = method[T, S](HttpMethods.HEAD, transformer)

  def connect[T, S]()(transformer: HttpResponse[T] => Future[S]): HttpClientTransformer[T, S] = method[T, S](HttpMethods.CONNECT, transformer)

  def trace[T, S]()(transformer: HttpResponse[T] => Future[S]): HttpClientTransformer[T, S] = method[T, S](HttpMethods.TRACE, transformer)

  def custom[T, S](custom: String)(transformer: HttpResponse[T] => Future[S]): HttpClientTransformer[T, S] =
    method[T, S](HttpMethods.CUSTOM(custom), transformer)

  def secure[T, S](transformer: HttpClientTransformer[T, S]): HttpClientTransformer[T, S] = {
    (client: HttpClient[T]) => {
      val wrappedClient: HttpClient[T] = new HttpClient[T] {
        def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = client(request)
        override def isDefinedAt(request: HttpRequest[T]) = request.scheme == "https"
      }
      transformer(wrappedClient)
    }
  }
  
  private def method[T, S](method: HttpMethod, transformer: HttpResponse[T] => Future[S], content: Option[T] = None) = {
    (client: HttpClient[T]) => {
      client.apply(HttpRequest[T](method = method, content = content, uri = "")).flatMap(transformer)
    }
  }

  private def apply[T, S](copy: (HttpRequest[T]) => HttpRequest[T])(transformer: HttpClientTransformer[T, S]): HttpClientTransformer[T, S] = {
    (client: HttpClient[T]) => {
      val wrappedClient = new HttpClient[T] {
        def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = client.apply(copy(request))
      }

      transformer(wrappedClient)
    }
  }
}