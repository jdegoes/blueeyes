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
  
  /**
   * <pre>
   * path("/foo") {
   *   ...
   * } ~ orFail { request => BadRequest -> "The path " + request.path + " is malformed" }
   * </pre>
   */
  def orFail[T, S](h: HttpRequest[T] => (HttpFailure, String)): HttpRequestHandler2[T, S] = new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = true
    
    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = {
      val fail = h(r)
      
      Future.dead(HttpException(fail._1, fail._2))
    }
  }
  
  /**
   * <pre>
   * path("/foo") {
   *   ...
   * } ~ orFail("The path is malformed")
   * </pre>
   */
  def orFail[T, S](msg: String): HttpRequestHandler2[T, S] = orFail { request => HttpStatusCodes.BadRequest -> msg }
  
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

  /**
   * Extracts data from the request. The extractor combinators can be used to 
   * factor out extraction logic that's duplicated across a range of paths or
   * methods.
   * <pre>
   * extract(_.parameters('username)) { username =>
   *   ...
   * }
   * </pre>
   */
  def extract[T, S, E1](extractor: HttpRequest[T] => E1)(h: E1 => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = {
      try {
        val extracted = extractor(r)
        
        h(extracted).isDefinedAt(r)
      }
      catch {
        case t: Throwable => throw HttpException(HttpStatusCodes.BadRequest, t)
      }
    }
    
    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = {
      val extracted = extractor(r)
      
      h(extracted).apply(r)
    }
  }
  
  /**
   * <pre>
   * extract(r => (r.parameters('username), r.parameters('password))) { (username, password) =>
   *   ...
   * }
   * </pre>
   */
  def extract[T, S, E1, E2](extractor: HttpRequest[T] => (E1, E2))(h: (E1, E2) => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = {
      try {
        val extracted = extractor(r)
        
        h(extracted._1, extracted._2).isDefinedAt(r)
      }
      catch {
        case t: Throwable => throw HttpException(HttpStatusCodes.BadRequest, t)
      }
    }
    
    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = {
      val extracted = extractor(r)
      
      h(extracted._1, extracted._2).apply(r)
    }
  }
  
  def extract[T, S, E1, E2, E3](extractor: HttpRequest[T] => (E1, E2, E3))(h: (E1, E2, E3) => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = {
      try {
        val extracted = extractor(r)
        
        h(extracted._1, extracted._2, extracted._3).isDefinedAt(r)
      }
      catch {
        case t: Throwable => throw HttpException(HttpStatusCodes.BadRequest, t)
      }
    }
    
    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = {
      val extracted = extractor(r)
      
      h(extracted._1, extracted._2, extracted._3).apply(r)
    }
  }
  
  def extract[T, S, E1, E2, E3, E4](extractor: HttpRequest[T] => (E1, E2, E3, E4))(h: (E1, E2, E3, E4) => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = {
      try {
        val extracted = extractor(r)
        
        h(extracted._1, extracted._2, extracted._3, extracted._4).isDefinedAt(r)
      }
      catch {
        case t: Throwable => throw HttpException(HttpStatusCodes.BadRequest, t)
      }
    }
    
    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = {
      val extracted = extractor(r)
      
      h(extracted._1, extracted._2, extracted._3, extracted._4).apply(r)
    }
  }
  
  def extract[T, S, E1, E2, E3, E4, E5](extractor: HttpRequest[T] => (E1, E2, E3, E4, E5))(h: (E1, E2, E3, E4, E5) => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = new HttpRequestHandler2[T, S] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = {
      try {
        val extracted = extractor(r)
        
        h(extracted._1, extracted._2, extracted._3, extracted._4, extracted._5).isDefinedAt(r)
      }
      catch {
        case t: Throwable => throw HttpException(HttpStatusCodes.BadRequest, t)
      }
    }
    
    def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = {
      val extracted = extractor(r)
      
      h(extracted._1, extracted._2, extracted._3, extracted._4, extracted._5).apply(r)
    }
  }
  
  /** A special-case extractor for parameters.
   * <pre>
   * parameter('token) { token =>
   *   get {
   *     ...
   *   }
   * }
   * </pre>
   */
  def parameter[T, S](s1: Symbol)(h: String => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = extract[T, S, String] { request =>
    request.parameters(s1)
  } { h }

  /** A special-case extractor for parameters.
   * <pre>
   * parameters('username, 'password) { (username, password) =>
   *   get {
   *     ...
   *   }
   * }
   * </pre>
   */  
  def parameters[T, S](s1: Symbol, s2: Symbol)(h: (String, String) => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = extract[T, S, String, String] { request =>
    (request.parameters(s1), request.parameters(s2))
  } { h }
  
  def parameters[T, S](s1: Symbol, s2: Symbol, s3: Symbol)(h: (String, String, String) => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = extract[T, S, String, String, String] { request =>
    (request.parameters(s1), request.parameters(s2), request.parameters(s3))
  } { h }
  
  def parameters[T, S](s1: Symbol, s2: Symbol, s3: Symbol, s4: Symbol)(h: (String, String, String, String) => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = extract[T, S, String, String, String, String] { request =>
    (request.parameters(s1), request.parameters(s2), request.parameters(s3), request.parameters(s4))
  } { h }
  
  def parameters[T, S](s1: Symbol, s2: Symbol, s3: Symbol, s4: Symbol, s5: Symbol)(h: (String, String, String, String, String) => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = extract[T, S, String, String, String, String, String] { request =>
    (request.parameters(s1), request.parameters(s2), request.parameters(s3), request.parameters(s4), request.parameters(s5))
  } { h }

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