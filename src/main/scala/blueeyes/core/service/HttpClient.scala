package blueeyes.core.service

import blueeyes.concurrent.Future
import blueeyes.core.http._

import blueeyes.core.data._

trait HttpClient[T] extends HttpRequestHandler[T] {
  def isDefinedAt(request: HttpRequest[T]): Boolean = true

  def apply(request: HttpRequest[T]): Future[HttpResponse[T]]

  def apply[R](f: HttpClient[T] => Future[R]) = f(this)
}

class HttpClientProxy[T](r: HttpRequestHandler[T]) {
  def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = r.apply(request)
}


trait  HttpClient2[A] extends HttpRequestHandler[A] { self =>
  def get(path: String) = self.apply(HttpRequest(method = HttpMethods.GET, uri = path))

  def post[B](path: String)(content: B)(implicit transcoder: Bijection[B, A]) = {
    self.apply(HttpRequest(method = HttpMethods.POST, uri = path, content = Some(transcoder.apply(content))))
  }

  def put[B](path: String)(content: B)(implicit transcoder: Bijection[B, A]) = {
    self.apply(HttpRequest(method = HttpMethods.PUT, uri = path, content = Some(transcoder.apply(content))))
  }

  def delete(path: String) = self.apply(HttpRequest(method = HttpMethods.DELETE, uri = path))

  def header(name: String, value: String): HttpClient2[A] = header((name, value))

  def header(h: HttpHeader): HttpClient2[A] = new HttpClient2[A] {
    def isDefinedAt(request: HttpRequest[A]): Boolean = self.isDefinedAt(request)

    def apply(request: HttpRequest[A]): Future[HttpResponse[A]] = self.apply {
      request.copy(headers = request.headers + h)
    }
  }

  def headers(h: Iterable[HttpHeader]): HttpClient2[A] = new HttpClient2[A] {
    def isDefinedAt(request: HttpRequest[A]): Boolean = self.isDefinedAt(request)

    def apply(request: HttpRequest[A]): Future[HttpResponse[A]] = self.apply {
      request.copy(headers = request.headers ++ (h: HttpHeaders))
    }
  }

  def query(name: String, value: String): HttpClient2[A] = queries((name, value))

  def queries(qs: (String, String)*): HttpClient2[A] = new HttpClient2[A] {
    def isDefinedAt(request: HttpRequest[A]): Boolean = self.isDefinedAt(request)

    def apply(request: HttpRequest[A]): Future[HttpResponse[A]] = self.apply {
      addQueries(request)(qs)
    }
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

    request.copy(uri = newUrl)
  }
}