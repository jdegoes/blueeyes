package blueeyes.core.service

import blueeyes.util.Future
//import blueeyes.core.http.{HttpVersion, HttpVersions, HttpMethod}
import blueeyes.core.http.HttpVersions._
import blueeyes.core.http.HttpVersion
import blueeyes.core.http.HttpMethods._
import blueeyes.core.http.HttpMethod

trait HttpClient[T] {
  def apply(request: HttpRequest[T]): Future[HttpResponse[T]]
  
  def get(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, remoteHost: Option[String] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] =
    apply(HttpRequest(GET, uri, parameters, buildHeaders(headers, remoteHost), content, None, version))
    
  def put(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, remoteHost: Option[String] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] =
    apply(HttpRequest(PUT, uri, parameters, buildHeaders(headers, remoteHost), content, None, version))
    
  def post(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, remoteHost: Option[String] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] =
    apply(HttpRequest(POST, uri, parameters, buildHeaders(headers, remoteHost), content, None, version))
    
  def delete(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, remoteHost: Option[String] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] =
    apply(HttpRequest(DELETE, uri, parameters, buildHeaders(headers, remoteHost), content, None, version))
    
  def options(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, remoteHost: Option[String] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] =
    apply(HttpRequest(OPTIONS, uri, parameters, buildHeaders(headers, remoteHost), content, None, version))

  def head(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, remoteHost: Option[String] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] =
    apply(HttpRequest(HEAD, uri, parameters, buildHeaders(headers, remoteHost), content, None, version))
  
  def connect(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, remoteHost: Option[String] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] =
    apply(HttpRequest(CONNECT, uri, parameters, buildHeaders(headers, remoteHost), content, None, version))

  def trace(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, remoteHost: Option[String] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] =
    apply(HttpRequest(TRACE, uri, parameters, buildHeaders(headers, remoteHost), content, None, version))
    
  def custom(method: HttpMethod, uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, remoteHost: Option[String] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] =
    apply(HttpRequest(method, uri, parameters, buildHeaders(headers, remoteHost), content, None, version))

  private def buildHeaders(originalHeaders: Map[String, String], remoteHost: Option[String]) = remoteHost.map(v => originalHeaders + Tuple2("X-Forwarded-For", v)).getOrElse(originalHeaders)
}
