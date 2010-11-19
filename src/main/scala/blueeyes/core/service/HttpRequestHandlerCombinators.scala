package blueeyes.core.service

import scala.xml.NodeSeq

import blueeyes.util.Future
import blueeyes.json.JsonAST._
import blueeyes.core.data.Bijection
import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpHeaderImplicits._

trait HttpRequestHandlerCombinators {
  /** The path combinator creates a handler that is defined only for suffixes 
   * of the specified path pattern.
   * <p>
   * <pre>
   * path("/foo") {
   *   ...
   * }
   * </pre>
   */
  def path[T, S](path: RestPathPattern)(h: HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = path.isDefinedAt(r.path) && h.isDefinedAt(path.shift(r))
    
    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = {
      val pathParameters = path(r.path)
      
      val shiftedRequest = path.shift(r)
      
      h(shiftedRequest.copy(parameters = shiftedRequest.parameters ++ pathParameters))
    }
  }
  
  /** The method combinator creates a handler that is defined only for the 
   * specified HTTP method.
   */
  def method[T, S](method: HttpMethod)(h: HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = r.method == method
    
    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = r.method match {
      case `method` => h(r)
      
      case _ => error("The handler " + h + " can only respond to HTTP method " + method)
    }
  }
  
  /** The path end combinator creates a handler that is defined only for paths 
   * that are fully matched.
   */
  def $ [T, S](h: HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = path(RestPathPatternParsers.EmptyPathPattern) { h }
  
  def get     [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.GET)      { toPartial(h) } }
  def put     [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.PUT)      { toPartial(h) } }
  def post    [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.POST)     { toPartial(h) } }
  def delete  [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.DELETE)   { toPartial(h) } }
  def head    [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.HEAD)     { toPartial(h) } }
  def patch   [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.PATCH)    { toPartial(h) } }
  def options [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.OPTIONS)  { toPartial(h) } }
  def trace   [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.TRACE)    { toPartial(h) } }
  def connect [T, S](h: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = $ { method(HttpMethods.CONNECT)  { toPartial(h) } }

  /** The accept combinator creates a handler that is defined only for requests
   * that have the specified content type. Requires an implicit bijection
   * used for transcoding.
   */
  def accept[T, S, U](mimeType: MimeType)(h: HttpRequestHandler2[T, S])(implicit b: Bijection[U, T]): HttpRequestHandler2[U, S] = new HttpRequestHandler2[U, S] {
    def isDefinedAt(r: HttpRequest[U]): Boolean = {
      val requestMimeType: List[MimeType] = (for (`Content-Type`(mimeTypes) <- r.headers) yield mimeTypes.toList).toList.flatten
      
      requestMimeType.find(_ == mimeType).map { mimeType =>
        h.isDefinedAt(r.copy(content = r.content.map(b.apply)))
      }.orElse {
        r.content.map(b.isDefinedAt _)
      }.getOrElse(false)
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
  def jvalue[T](h: HttpRequestHandler[JValue])(implicit b: Bijection[T, JValue]): HttpRequestHandler[T] = contentType(MimeTypes.application/MimeTypes.json) { h }
  
  /** The xml combinator creates a handler that accepts and produces XML. 
   * Requires an implicit bijection used for transcoding.
   */
  def xml[T](h: HttpRequestHandler[NodeSeq])(implicit b: Bijection[T, NodeSeq]): HttpRequestHandler[T] = contentType(MimeTypes.text/MimeTypes.xml) { h }
  
  private def toPartial[T, S](full: HttpRequestHandlerFull2[T, S]): HttpRequestHandler2[T, S] = new HttpRequestHandler2[T, S] {
    def isDefinedAt(request: HttpRequest[T]) = true
  
    def apply(request: HttpRequest[T]): Future[HttpResponse[S]] = full.apply(request)
  }
}