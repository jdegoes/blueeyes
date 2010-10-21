package blueeyes.core.service

import blueeyes.util.Future
//import blueeyes.core.http.{HttpVersion, HttpVersions, HttpMethod}
import blueeyes.core.http.HttpVersions._
import blueeyes.core.http.HttpVersion
import blueeyes.core.http.HttpMethods._
import blueeyes.core.http.HttpMethod

trait HttpClient[T] {
  def apply(request: HttpRequest[T]): Future[HttpResponse[T]]
  
  def get(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] = 
    apply(HttpRequest(GET, uri, parameters, headers, content, version))
    
  def put(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] = 
    apply(HttpRequest(PUT, uri, parameters, headers, content, version))
    
  def post(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] = 
    apply(HttpRequest(POST, uri, parameters, headers, content, version))
    
  def delete(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] = 
    apply(HttpRequest(DELETE, uri, parameters, headers, content, version))
    
  def options(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] = 
    apply(HttpRequest(OPTIONS, uri, parameters, headers, content, version))

  def head(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] = 
    apply(HttpRequest(HEAD, uri, parameters, headers, content, version))
  
  def connect(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] = 
    apply(HttpRequest(CONNECT, uri, parameters, headers, content, version))

  def trace(uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] = 
    apply(HttpRequest(TRACE, uri, parameters, headers, content, version))
    
  def custom(method: HttpMethod, uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, version: HttpVersion = `HTTP/1.1`): Future[HttpResponse[T]] = 
    apply(HttpRequest(method, uri, parameters, headers, content, version))
}
