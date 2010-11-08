package blueeyes.core.service

import blueeyes.core.http.HttpStatusImplicits._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpHeaderImplicits._
import blueeyes.core.http.{HttpStatusCodes, HttpRequest, HttpResponse, HttpStatus, HttpMethod, HttpException, HttpHeader}
import blueeyes.util.Future

/** A handler for a particular http method (get/post/put/delete/etc.). A 
 * handler is a partial function from request to response.
 */
//sealed trait HttpRequestHandler[T] extends PartialFunction[HttpRequest[T], Future[HttpResponse[T]]]

/** A handler that transcodes data to a subhandler that expects/produces data 
 * in a different format.
 */
sealed trait HttpTranscodingRequestHandler[In, Out, Base] extends HttpRequestHandler[Base] {
  /** The request handler, which must return a future of the response.
   */
  def handler: HttpRequest[In] => Future[HttpResponse[Out]]

  /** The input transcoder, which will convert data from the input request type
   * to the type required by the handler.
   */
  def in: HttpDataTranscoder[Base, In]

  /** The output transcoder, which will convert data from the type produced by
   * the handler to the output request type.
   */
  def out: HttpDataTranscoder[Out, Base]
  
  def apply(request: HttpRequest[Base]): Future[HttpResponse[Base]] = {
    val decodedContent = try {
      request.content.map(in.transcode)
    } catch {
      case e: Exception => return Future.dead(HttpException(UnsupportedMediaType, e))
    }
    
    val newRequest = request.copy(content = decodedContent)
    
    handler(newRequest).map { response =>
      response.copy(headers = response.headers + `Content-Type`(out.mimeType), content = response.content.map(out.transcode))
    }
  }
}

sealed case class HttpNoHandler[T]() extends HttpRequestHandler[T] {
  def isDefinedAt(request: HttpRequest[T]): Boolean = false
  
  def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = error("This handler is not defined anywhere")
}

sealed case class HttpPathMethodHandler[In, Out, Base](/** The pattern of paths handled by the request handler.
                                                         */
                                                        path: RestPathPattern, 
  
                                                        /** The HTTP method handled by the request handler.
                                                         */
                                                        method: HttpMethod, 

                                                        handler: HttpRequest[In] => Future[HttpResponse[Out]], 
                                                        
                                                        in: HttpDataTranscoder[Base, In],
  
                                                        out: HttpDataTranscoder[Out, Base]) extends HttpTranscodingRequestHandler[In, Out, Base] { self =>
  def isDefinedAt(request: HttpRequest[Base]): Boolean = method == request.method && path.isDefinedAt(request.path)
    
  override def apply(request: HttpRequest[Base]): Future[HttpResponse[Base]] = {
    val pathParameters = path(request.path)
    
    val newRequest = request.copy(parameters = request.parameters ++ pathParameters)
    
    super.apply(newRequest)
  }
}

/** A not found handler. The not found handler always returns the NotFound HTTP status.
 */
sealed case class HttpNotFoundHandler[In, Out, Base](handler: HttpRequest[In] => Future[HttpResponse[Out]], in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]) extends HttpTranscodingRequestHandler[In, Out, Base] { self =>
  def isDefinedAt(request: HttpRequest[Base]): Boolean = true
  
  override def apply(request: HttpRequest[Base]): Future[HttpResponse[Base]] = {
    super.apply(request).map { response =>
      if (response.status.code != NotFound) response.copy(status = HttpStatus(NotFound)) else response
    }
  }
}