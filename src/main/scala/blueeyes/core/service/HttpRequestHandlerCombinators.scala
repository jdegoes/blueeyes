package blueeyes.core.service

import scala.xml.NodeSeq

import blueeyes.util.Future
import blueeyes.json.JsonAST._
import blueeyes.core.data.Bijection
import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpHeaderImplicits._

trait HttpRequestHandlerCombinators {
  /** The path combinator is defined only for matches to the specified path 
   * pattern.
   * <p>
   * <pre>
   * path("/foo") {
   *   ...
   * }
   * </pre>
   */
  def path[T](path: RestPathPattern)(h: HttpRequestHandler[T]): HttpRequestHandler[T] = new HttpRequestHandler[T] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = path.isDefinedAt(r.path) && h.isDefinedAt(path.shift(r))
    
    def apply(r: HttpRequest[T]): Future[HttpResponse[T]] = {
      val pathParameters = path(r.path)
      
      val shiftedRequest = path.shift(r)
      
      h(shiftedRequest.copy(parameters = shiftedRequest.parameters ++ pathParameters))
    }
  }
  
  /** The method combinator created a handler that is defined only for the 
   * specified HTTP method.
   */
  def method[T](method: HttpMethod)(h: HttpRequestHandler[T]): HttpRequestHandler[T] = new HttpRequestHandler[T] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = r.method == method
    
    def apply(r: HttpRequest[T]): Future[HttpResponse[T]] = r.method match {
      case `method` => h(r)
      
      case _ => error("The handler " + h + " can only respond to HTTP method " + method)
    }
  }
  
  /** The path end combinator creates a handler that is defined only for paths 
   * that are fully matched.
   */
  def $ [T](h: HttpRequestHandler[T]): HttpRequestHandler[T] = path(RestPathPatternParsers.EmptyPathPattern) { h }
  
  def get     [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.GET)      { toPartial(h) } }
  def put     [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.PUT)      { toPartial(h) } }
  def post    [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.POST)     { toPartial(h) } }
  def delete  [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.DELETE)   { toPartial(h) } }
  def head    [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.HEAD)     { toPartial(h) } }
  def patch   [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.PATCH)    { toPartial(h) } }
  def options [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.OPTIONS)  { toPartial(h) } }
  def trace   [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.TRACE)    { toPartial(h) } }
  def connect [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.CONNECT)  { toPartial(h) } }

  /** The accept combinator creates a handler that is defined only for requests
   * that have the specified content type. Requires an implicit bijection
   * used for transcoding.
   */
  def accept[T, S, U](mimeType: MimeType)(h: HttpRequestHandler2[T, S])(implicit b: Bijection[U, T]): HttpRequestHandler2[U, S] = new HttpRequestHandler2[U, S] {
    def isDefinedAt(r: HttpRequest[U]): Boolean = {
      val requestMimeType = (for (`Content-Type`(mimeTypes) <- r.headers) yield `Content-Type`(mimeTypes: _*)).headOption
      
      requestMimeType match {
        case Some(`mimeType`) => h.isDefinedAt(r.copy(content = r.content.map(b.apply)))
        
        case _ => false
      }
    }
    
    def apply(r: HttpRequest[U]) = h(r.copy(content = r.content.map(b.apply)))
  }
  
  /** The produce combinator creates a handler that is produces responses 
   * that have the specified content type. Requires an implicit bijection
   * used for transcoding.
   */
  def produce[T, S, V](mimeType: MimeType)(h: HttpRequestHandler2[T, S])(implicit b: Bijection[S, V]): HttpRequestHandler2[T, V] = new HttpRequestHandler2[T, V] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = h.isDefinedAt(r)
    
    def apply(r: HttpRequest[T]): Future[HttpResponse[V]] = h(r).map { response =>
      response.copy(content = response.content.map(b.apply), headers = response.headers + `Content-Type`(mimeType))
    }
  }
  
  /** The content type combinator creates a handler that accepts and produces
   * requests and responses of the specified content type. Requires an implicit
   * bijection used for transcoding.
   */
  def contentType[T, S](mimeType: MimeType)(h: HttpRequestHandler[T])(implicit b1: Bijection[S, T]): HttpRequestHandler[S] = {
    implicit val b2 = b1.inverse
    
    accept(mimeType) {
      produce(mimeType) {
        h
      }
    }
  }
  
  /** The json combinator creates a handler that accepts and produces JSON. 
   * Requires an implicit bijection used for transcoding.
   */
  def json[T](h: HttpRequestHandler[JValue])(implicit b: Bijection[T, JValue]): HttpRequestHandler[T] = contentType(MimeTypes.application/MimeTypes.json) { h }
  
  /** The xml combinator creates a handler that accepts and produces XML. 
   * Requires an implicit bijection used for transcoding.
   */
  def xml[T](h: HttpRequestHandler[NodeSeq])(implicit b: Bijection[T, NodeSeq]): HttpRequestHandler[T] = contentType(MimeTypes.text/MimeTypes.xml) { h }
  
  private def toPartial[T](full: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = new HttpRequestHandler[T] {
    def isDefinedAt(request: HttpRequest[T]) = true
  
    def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = full.apply(request)
  }
}