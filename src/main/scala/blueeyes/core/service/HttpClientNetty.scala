package blueeyes.core.service

import HttpStatusCodeImplicits._
import blueeyes.core.data.{Bijection, DataTranscoder}
import blueeyes.util.Future
import com.ning.http.client.{ 
	AsyncHttpClient, 
	AsyncCompletionHandler, 
	FluentStringsMap, 
	Response, 
	RequestBuilderBase 
}
import java.util.concurrent.{ Future => JavaFuture }

trait HttpClientNetty[T] extends HttpClient[T] with DataTranscoder[T, String] {
  def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = {
    new Future[HttpResponse[T]]() {
      prepareRequest(request).get.execute(
        new AsyncCompletionHandler[Response] {
          def onCompleted(response: Response): Response = {
            val data = transcode.unapply(response.getResponseBody)
            deliver(HttpResponse[T](status = HttpStatus(response.getStatusCode), content = Some(data)));
            response;
          }

          override def onThrowable(t: Throwable) {
            deliver(HttpResponse[T](HttpStatus(HttpStatusCodes.InternalServerError)));
          }
        })
    }
  }

  /**
   * TODO - Figure out how set set HTTP version using Netty
   * @param request
   * @return
   */
  protected def prepareRequest(request: HttpRequest[T]): Option[AsyncHttpClient#BoundRequestBuilder] = {	
    def setBody(rb: Option[AsyncHttpClient#BoundRequestBuilder]): Option[AsyncHttpClient#BoundRequestBuilder] = {
      for (content <- request.content; requestBuilder <- rb)
        yield requestBuilder.setBody(transcode(content))
    }

    /**
    * Netty does not allow parameters to be set for non-POST/PUT requests
    */
    def setParameters(requestBuilder: Option[AsyncHttpClient#BoundRequestBuilder]): Option[AsyncHttpClient#BoundRequestBuilder] = {
      requestBuilder.map(rb => { 
        rb.setParameters(request.parameters.foldLeft(new FluentStringsMap()) { (fsMap, pair) => 
          fsMap.add(pair._1, pair._2)
        })
      })
    }
	
    var reqBuilder = request.method match {
      case HttpMethods.GET      => Some(new AsyncHttpClient().prepareGet(request.uri))
      case HttpMethods.POST     => setParameters(setBody(Some(new AsyncHttpClient().preparePost(request.uri))))
      case HttpMethods.PUT      => setParameters(setBody(Some(new AsyncHttpClient().preparePut(request.uri))))
      case HttpMethods.DELETE   => Some(new AsyncHttpClient().prepareDelete(request.uri))
      case HttpMethods.OPTIONS  => Some(new AsyncHttpClient().prepareOptions(request.uri))
      case _ => None
    }

    // ("Content-Type", "text/javascript")
    val newHeaders = request.headers /*++ Map(
      ContentType((for (ContentType(contentType) <- request.headers) yield contentType).headOption.getOrElse(transcoder.mimeType._2)),
      ContentLength(): _*
    )*/
    
    //val contentType = 

    for (pair <- newHeaders; r <- reqBuilder)
      yield r.setHeader(pair._1, pair._2)   
    
    reqBuilder
  }
}

