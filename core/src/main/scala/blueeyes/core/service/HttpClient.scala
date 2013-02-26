package blueeyes.core.service

import akka.dispatch.Future
import blueeyes.core.http._
import blueeyes.core.data._
import java.net.InetAddress
import org.jboss.netty.handler.codec.http.CookieEncoder

import scalaz._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.syntax.semigroup._

trait HttpClient[A] extends HttpClientHandler[A] { self =>
  def get[B](path: String)(implicit transcoder: AsyncHttpTranscoder[B, A]) = method[B](HttpMethods.GET, path)

  def post[B](path: String)(content: B)(implicit f: B => A, transcoder: AsyncHttpTranscoder[B, A]) = method[B](HttpMethods.POST, path, Some(f(content)))

  def put[B](path: String)(content: B)(implicit f: B => A, transcoder: AsyncHttpTranscoder[B, A]) = method[B](HttpMethods.PUT, path, Some(f(content)))

  def delete[B](path: String)(implicit transcoder: AsyncHttpTranscoder[B, A]) = method[B](HttpMethods.DELETE, path)

  def options[B](path: String)(implicit transcoder: AsyncHttpTranscoder[B, A]) = method[B](HttpMethods.OPTIONS, path)

  def head[B](path: String)(implicit transcoder: AsyncHttpTranscoder[B, A]) = method[B](HttpMethods.HEAD, path)

  def connect[B](path: String)(implicit transcoder: AsyncHttpTranscoder[B, A]) = method[B](HttpMethods.CONNECT, path)

  def trace[B](path: String)(implicit transcoder: AsyncHttpTranscoder[B, A]) = method[B](HttpMethods.TRACE, path)

  def custom[B](custom: HttpMethod, path: String)(implicit transcoder: AsyncHttpTranscoder[B, A]) = method[B](custom, path)

  def protocol(protocol: String) = buildClient { request => 
    request.withUriChanges(scheme = Some(protocol)) 
  }

  def secure = protocol("https")

  def host(host: String) = buildClient { request => 
    request.withUriChanges(host = Some(host)) 
  }

  def port(port: Int) = buildClient { request => 
    request.withUriChanges(port = Some(port)) 
  }

  // This is extremely dodgy. Basically, the existing semantics have been that the path combinator
  // is an all-singing, all-dancing utility for building URIs. This is at least very slightly more
  // sane than what was here before.
  def path(path: String) = buildClient { request =>
    def sep(o1: Option[String], o2: Option[String], s: String) = {
      o1 |+| Apply[Option].apply2(o1, o2) { (_, _) => s } |+| o2
    }

    val parsed = URI(path)
    val uri0 = request.uri.copy(
      scheme = parsed.scheme.orElse(request.uri.scheme),
      userInfo = parsed.userInfo.orElse(request.uri.userInfo),
      host = parsed.host.orElse(request.uri.host),
      port = parsed.port.orElse(request.uri.port),
      path = sep(parsed.path, request.uri.path, "/") map { _.replaceAll("/+", "/") },
      query = sep(parsed.query, request.uri.query, "&") map { _.replaceAll("&+", "&") },
      fragment = parsed.fragment.orElse(request.uri.fragment)
    )
    request.copy(uri = uri0, subpath = uri0.path.getOrElse(""))
  }

  def parameters(parameters: (Symbol, String)*) = buildClient { request => 
    request.copy(parameters = Map[Symbol, String](parameters: _*))
  }

  def content[B](content: B)(implicit projection: B => A) = buildClient { request => 
    request.copy(content = Some(projection(content)))
  }

  def cookies(cookies: (String, String)*) = buildClient { request =>
    val cookieEncoder = new CookieEncoder(false)
    cookies.foreach(cookie => cookieEncoder.addCookie(cookie._1, cookie._2))

    request.copy(headers = request.headers + Tuple2("Cookie", cookieEncoder.encode()))
  }

  def remoteHost(host: InetAddress) = buildClient { request => 
    request.copy(
      headers = request.headers + Tuple2("X-Forwarded-For", host.getHostAddress) + Tuple2("X-Cluster-Client-Ip", host.getHostAddress), 
      remoteHost = Some(host)
    )
  }

  def header(name: String, value: String): HttpClient[A] = header((name, value))

  def header(h: HttpHeader): HttpClient[A] = buildClient { request => 
    request.copy(headers = request.headers + h)
  }

  def headers(h: Iterable[HttpHeader]): HttpClient[A] = buildClient { request => 
    request.copy(headers = request.headers ++ h) 
  }

  def version(version: HttpVersion) = buildClient { request => 
    request.copy(version = version)
  }

  def query(name: String, value: String): HttpClient[A] = queries((name, value))

  def queries(qs: (String, String)*): HttpClient[A] = buildClient { request => addQueries(request)(qs) }

  def contentType[B](mimeType: MimeType)(implicit transcoder: AsyncHttpTranscoder[B, A]) = new HttpClient[B] {
    private def contentHeader = ("Content-Type", mimeType.value)
    def isDefinedAt(request: HttpRequest[B]): Boolean = 
      self.isDefinedAt(transcoder(request).copy(headers = request.headers + contentHeader))

    def apply(request: HttpRequest[B]): Future[HttpResponse[B]] = 
      transcoder.unapply(self.apply(transcoder(request).copy(headers = request.headers + contentHeader)))
  }

  def translate[B](implicit transcoder: AsyncHttpTranscoder[B, A]) = new HttpClient[B] {
    def isDefinedAt(request: HttpRequest[B]): Boolean = self.isDefinedAt(transcoder(request))
    def apply(request: HttpRequest[B]): Future[HttpResponse[B]] = transcoder.unapply(self.apply(transcoder(request)))
  }

  private def addQueries(request: HttpRequest[A])(queries: Iterable[(String, String)]): HttpRequest[A] = {
    import java.net.URLEncoder

    val url = request.uri.toString
    val qs  = queries.map(t => t._1 + "=" + URLEncoder.encode(t._2, "UTF-8")).mkString("&")

    val index = url.indexOf('?')

    val newUrl = (if (index >= 0) {
      if (index == url.length - 1) url + qs
      else url + "&" + qs
    }
    else url + "?" + qs)

    HttpRequest(request.method, URI(newUrl), request.parameters, request.headers, request.content, request.remoteHost, request.version)
  }

  private def method[B](method: HttpMethod, path: String, content: Option[A] = None)(implicit transcoder: AsyncHttpTranscoder[B, A]): Future[HttpResponse[B]] = 
    transcoder.unapply(self.apply(HttpRequest(method, path,  Map(),  Map(), content)))

  private def buildClient(copy: (HttpRequest[A]) => HttpRequest[A]) = new HttpClient[A] {
    def isDefinedAt(request: HttpRequest[A]): Boolean = self.isDefinedAt(request)

    def apply(request: HttpRequest[A]): Future[HttpResponse[A]] = self.apply {
      copy(request)
    }
  }
}

object HttpClient extends blueeyes.bkka.AkkaDefaults {
  implicit def requestHandlerToHttpClient[A](h: HttpClientHandler[A]): HttpClient[A] = new HttpClient[A] {
    def isDefinedAt(r: HttpRequest[A]): Boolean = h.isDefinedAt(r)

    def apply(r: HttpRequest[A]): Future[HttpResponse[A]] = h.apply(r)
  }

  class EchoClient[T](f: HttpRequest[T] => Option[T]) extends HttpClient[T] {
    override def apply(r: HttpRequest[T]) = Future(HttpResponse[T](content = f(r)))

    override def isDefinedAt(x: HttpRequest[T]) = true
  }
}
