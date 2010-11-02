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
sealed trait HttpRequestHandler[T] extends PartialFunction[HttpRequest[T], Future[HttpResponse[T]]]

sealed trait HttpTranscodingRequestHandler[In, Out, Base] extends HttpRequestHandler[Base] {
  /** The request handler, which must return a future of the response.
   */
  def handler: HttpRequest[In] => Future[HttpResponse[Out]]

  /** The transcoder, which will convert data from the response type to a base
   * type, which is used by the underlying implementation.
   */
  def inTranscoder: HttpDataTranscoder[Base, In]
  
  def outTranscoder: HttpDataTranscoder[Out, Base]
  
  def apply(request: HttpRequest[Base]): Future[HttpResponse[Base]] = {
    val decodedContent = try {
      request.content.map(inTranscoder.transcode)
    } catch {
      case e: Exception => return Future.dead(HttpException(UnsupportedMediaType, e))
    }
    
    val newRequest = request.copy(content = decodedContent)
    
    handler(newRequest).map { response =>
      response.copy(headers = response.headers + `Content-Type`(outTranscoder.mimeType), content = response.content.map(outTranscoder.transcode))
    }
  }
}

sealed case class HttpPathMethodHandler[In, Out, Base](/** The pattern of paths handled by the request handler.
                                                         */
                                                        path: RestPathPattern, 
  
                                                        /** The HTTP method handled by the request handler.
                                                         */
                                                        method: HttpMethod, 
  
                                                        /** The request handler, which must return a future of the response.
                                                         */
  
                                                        handler: HttpRequest[In] => Future[HttpResponse[Out]], 
                                                        
                                                        inTranscoder: HttpDataTranscoder[Base, In],
  
                                                        outTranscoder: HttpDataTranscoder[Out, Base]) extends HttpTranscodingRequestHandler[In, Out, Base] { self =>
  def isDefinedAt(request: HttpRequest[Base]): Boolean = method == request.method && path.isDefinedAt(request.path)
    
  override def apply(request: HttpRequest[Base]): Future[HttpResponse[Base]] = {
    val pathParameters = path(request.path)
    
    val newRequest = request.copy(parameters = request.parameters ++ pathParameters)
    
    super.apply(newRequest)
  }
}

sealed case class HttpNotFoundMethodHandler[In, Out, Base](handler: HttpRequest[In] => Future[HttpResponse[Out]], inTranscoder: HttpDataTranscoder[Base, In], outTranscoder: HttpDataTranscoder[Out, Base]) extends HttpTranscodingRequestHandler[In, Out, Base] { self =>
  def isDefinedAt(request: HttpRequest[Base]): Boolean = true
}