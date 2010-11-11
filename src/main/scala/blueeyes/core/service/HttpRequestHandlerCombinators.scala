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
  def transcode[In, Out, Base](h: HttpRequestHandlerFull2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = new HttpRequestHandlerFull[Base] {
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
  
  def get     [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.GET)      { toPartial(h) } }
  def put     [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.PUT)      { toPartial(h) } }
  def post    [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.POST)     { toPartial(h) } }
  def delete  [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.DELETE)   { toPartial(h) } }
  def head    [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.HEAD)     { toPartial(h) } }
  def patch   [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.PATCH)    { toPartial(h) } }
  def options [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.OPTIONS)  { toPartial(h) } }
  def trace   [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.TRACE)    { toPartial(h) } }
  def connect [T](h: HttpRequestHandlerFull[T]): HttpRequestHandler[T] = $ { method(HttpMethods.CONNECT)  { toPartial(h) } }
  
  def get[In, Out, Base](h: HttpRequestHandlerFull2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = get[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def put[In, Out, Base](h: HttpRequestHandlerFull2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = put[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def post[In, Out, Base](h: HttpRequestHandlerFull2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = post[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def delete[In, Out, Base](h: HttpRequestHandlerFull2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = delete[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def head[In, Out, Base](h: HttpRequestHandlerFull2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = head[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def patch[In, Out, Base](h: HttpRequestHandlerFull2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = patch[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def options[In, Out, Base](h: HttpRequestHandlerFull2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = options[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def trace[In, Out, Base](h: HttpRequestHandlerFull2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = trace[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  def connect[In, Out, Base](h: HttpRequestHandlerFull2[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): HttpRequestHandler[Base] = connect[Base] { 
    transcode[In, Out, Base] {      
      h
    }
  }
  
  private def toPartial[T](full: HttpRequestHandlerFull[T]) = new HttpRequestHandler[T] {
    def isDefinedAt(request: HttpRequest[T]) = true
  
    def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = full.apply(request)
  }
}
object HttpRequestHandlerCombinators extends HttpRequestHandlerCombinators