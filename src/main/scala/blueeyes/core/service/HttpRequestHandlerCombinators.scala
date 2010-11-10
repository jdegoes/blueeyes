package blueeyes.core.service

import blueeyes.util.Future
import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpHeaderImplicits._

trait HttpRequestHandlerCombinators {
  def path[T](path: RestPathPattern)(h: HttpRequestHandler[T]): HttpRequestHandler[T] = new HttpRequestHandler[T] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = path.isDefinedAt(r.path)
    
    def apply(r: HttpRequest[T]): Future[HttpResponse[T]] = {
      val pathParameters = path(r.path)
      
      val shiftedRequest = path.shift(r)
      
      h(shiftedRequest.copy(parameters = shiftedRequest.parameters ++ pathParameters))
    }
  }
  def transcode[In, Out, Base](h: HttpRequestHandler2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = new HttpRequestHandler[Base] {
    def isDefinedAt(r: HttpRequest[Base]): Boolean = true
    
    def apply(r: HttpRequest[Base]) = h(r.copy(content = r.content.map(in.transcode))).map { response =>
      response.copy(content = response.content.map(out.transcode), headers = response.headers + `Content-Type`(out.mimeType))
    }
  }
  def method[T](method: HttpMethod)(h: HttpRequestHandler[T]): HttpRequestHandler[T] = new HttpRequestHandler[T] {
    def isDefinedAt(r: HttpRequest[T]): Boolean = r.method == method
    
    def apply(r: HttpRequest[T]): Future[HttpResponse[T]] = r.method match {
      case `method` => h(r)
      
      case _ => error("The handler " + h + " can only respond to HTTP method " + method)
    }
  }
  def $[T](h: HttpRequestHandler[T]): HttpRequestHandler[T] = path(RestPathPatternParsers.EndPathPattern) { h }
  
  def get     [T](h: HttpRequestHandler[T]): HttpRequestHandler[T] = $ { method(HttpMethods.GET)      { h } }
  def put     [T](h: HttpRequestHandler[T]): HttpRequestHandler[T] = $ { method(HttpMethods.PUT)      { h } }
  def post    [T](h: HttpRequestHandler[T]): HttpRequestHandler[T] = $ { method(HttpMethods.POST)     { h } }
  def delete  [T](h: HttpRequestHandler[T]): HttpRequestHandler[T] = $ { method(HttpMethods.DELETE)   { h } }
  def head    [T](h: HttpRequestHandler[T]): HttpRequestHandler[T] = $ { method(HttpMethods.HEAD)     { h } }
  def patch   [T](h: HttpRequestHandler[T]): HttpRequestHandler[T] = $ { method(HttpMethods.PATCH)    { h } }
  def options [T](h: HttpRequestHandler[T]): HttpRequestHandler[T] = $ { method(HttpMethods.OPTIONS)  { h } }
  def trace   [T](h: HttpRequestHandler[T]): HttpRequestHandler[T] = $ { method(HttpMethods.TRACE)    { h } }
  def connect [T](h: HttpRequestHandler[T]): HttpRequestHandler[T] = $ { method(HttpMethods.CONNECT)  { h } }
  
  def get[In, Out, Base](h: HttpRequestHandler2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = get[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def put[In, Out, Base](h: HttpRequestHandler2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = put[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def post[In, Out, Base](h: HttpRequestHandler2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = post[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def delete[In, Out, Base](h: HttpRequestHandler2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = delete[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def head[In, Out, Base](h: HttpRequestHandler2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = head[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def patch[In, Out, Base](h: HttpRequestHandler2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = patch[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def options[In, Out, Base](h: HttpRequestHandler2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = options[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def trace[In, Out, Base](h: HttpRequestHandler2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = trace[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def connect[In, Out, Base](h: HttpRequestHandler2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = connect[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
}
object HttpRequestHandlerCombinators extends HttpRequestHandlerCombinators