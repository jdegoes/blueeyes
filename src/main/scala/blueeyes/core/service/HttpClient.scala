package blueeyes.core.service

import blueeyes.concurrent.Future
import blueeyes.core.http._
import blueeyes.core.data._
import java.net.InetAddress
import org.jboss.netty.handler.codec.http.CookieEncoder

trait HttpClient[A] extends HttpRequestHandler[A] { self =>

  def get(path: String) = method(HttpMethods.GET, path)

  def post[B](path: String)(content: B)(implicit transcoder: Bijection[B, A]) = method(HttpMethods.POST, path, Some(transcoder.apply(content)))

  def put[B](path: String)(content: B)(implicit transcoder: Bijection[B, A]) = method(HttpMethods.PUT, path, Some(transcoder.apply(content)))

  def delete(path: String) = method(HttpMethods.DELETE, path)

  def options(path: String) = method(HttpMethods.OPTIONS, path)

  def head(path: String) = method(HttpMethods.HEAD, path)

  def connect(path: String) = method(HttpMethods.CONNECT, path)

  def trace(path: String) = method(HttpMethods.TRACE, path)

  def custom(custom: HttpMethod, path: String) = method(custom, path)

  def protocol(protocol: String) = path(protocol + "://")

  def secure = protocol("https")

  def host(host: String) = path(host)

  def port(port: Int) = buildClient { request => request.copy(uri = ":" + port.toString + request.uri) }

  def path(path: String) = buildClient { request => HttpRequest(request.method, path + request.uri, request.parameters, request.headers, request.content, request.remoteHost, request.version) }

  def parameters(parameters: (Symbol, String)*) = buildClient { request => request.copy(parameters = Map[Symbol, String](parameters: _*))}

  def content[B](content: B)(implicit transcoder: Bijection[B, A]) = buildClient { request => request.copy(content = Some(transcoder.apply(content)))}

  def cookies(cookies: (String, String)*) = buildClient { request =>
    val cookieEncoder = new CookieEncoder(false)
    cookies.foreach(cookie => cookieEncoder.addCookie(cookie._1, cookie._2))

    request.copy(headers = request.headers + Tuple2("Cookie", cookieEncoder.encode()))
  }

  def remoteHost(host: InetAddress) = buildClient { request => HttpRequest(request.method, request.uri, request.parameters, request.headers + Tuple2("X-Forwarded-For", host.getHostAddress()), request.content, Some(host), request.version)}

  def header(name: String, value: String): HttpClient[A] = header((name, value))

  def header(h: HttpHeader): HttpClient[A] = buildClient {
    request => request.copy(headers = request.headers + h)
  }

  def headers(h: Iterable[HttpHeader]): HttpClient[A] = buildClient { request => request.copy(headers = request.headers ++ (h: HttpHeaders)) }

  def version(version: HttpVersion) = buildClient { request => HttpRequest(request.method, request.uri, request.parameters, request.headers, request.content, request.remoteHost, version)}

  def query(name: String, value: String): HttpClient[A] = queries((name, value))

  def queries(qs: (String, String)*): HttpClient[A] = buildClient { request => addQueries(request)(qs) }

  def contentType[B](mimeType: MimeType)(implicit transcoder: Bijection[B, A]) = new HttpClient[B] {
    def isDefinedAt(request: HttpRequest[B]): Boolean = self.isDefinedAt(request.copy(content = request.content.map(transcoder.apply(_))))

    def apply(request: HttpRequest[B]): Future[HttpResponse[B]] = self.apply {
      request.copy(content = request.content.map(transcoder.apply(_)), headers = request.headers + Tuple2("Content-Type", mimeType.value))
    } map {response => {
      val newC = response.content.map(transcoder.unapply(_))
      response.copy(content = response.content.map(transcoder.unapply(_)))
    }}
  }

  private def addQueries(request: HttpRequest[A])(queries: Iterable[(String, String)]): HttpRequest[A] = {
    import java.net.URLEncoder

    val url = request.uri
    val qs  = queries.map(t => t._1 + "=" + URLEncoder.encode(t._2, "UTF-8")).mkString("&")

    val index = url.indexOf('?')

    val newUrl = (if (index >= 0) {
      if (index == url.length - 1) url + qs
      else url + "&" + qs
    }
    else url + "?" + qs)

    HttpRequest(request.method, newUrl, request.parameters, request.headers, request.content, request.remoteHost, request.version)
  }

  private def method(method: HttpMethod, path: String, content: Option[A] = None) = self.apply(HttpRequest(method, path,  Map(),  Map(), content))

  private def buildClient(copy: (HttpRequest[A]) => HttpRequest[A]) = new HttpClient[A] {
    def isDefinedAt(request: HttpRequest[A]): Boolean = self.isDefinedAt(request)

    def apply(request: HttpRequest[A]): Future[HttpResponse[A]] = self.apply {
      copy(request)
    }
  }
}

object HttpClient {
  implicit def requestHandlerToHttpClient[A](h: HttpRequestHandler[A]): HttpClient[A] = new HttpClient[A] {
    def isDefinedAt(r: HttpRequest[A]): Boolean = h.isDefinedAt(r)

    def apply(r: HttpRequest[A]): Future[HttpResponse[A]] = h.apply(r)
  }
}