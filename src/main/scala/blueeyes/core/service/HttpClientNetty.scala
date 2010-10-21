package blueeyes.core.service

import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpStatusCodeImplicits._
import blueeyes.core.http.HttpHeaderImplicits._
import blueeyes.core.http.HttpStatusCodes
import blueeyes.core.http.HttpStatusCode
import blueeyes.core.http.HttpMethods
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.MimeType

import blueeyes.core.data.{ Bijection, DataTranscoder }
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
      prepareRequest(request).foreach(r => r.execute(
        new AsyncCompletionHandler[Response] {
          def onCompleted(response: Response): Response = {
            val data = transcode.unapply(response.getResponseBody)
            deliver(HttpResponse[T](status = HttpStatus(response.getStatusCode), content = Some(data)));
            response;
          }

          override def onThrowable(t: Throwable) {
            val httpStatus = HttpStatus(t match {
              case _:java.net.ConnectException => HttpStatusCodes.ServiceUnavailable
              case _ => HttpStatusCodes.InternalServerError
            })
            cancel(new Error(t))
          }
        }))
    }
  }

  /**
   * TODO - Figure out how set set HTTP version using Netty
   * @param request
   * @return
   */
  protected def prepareRequest(request: HttpRequest[T]): Option[AsyncHttpClient#BoundRequestBuilder] = {
    def setBody(requestBuilder: AsyncHttpClient#BoundRequestBuilder): AsyncHttpClient#BoundRequestBuilder = {
      (for (content <- request.content)  
        yield requestBuilder.setBody(transcode(content))).getOrElse(requestBuilder)
    }

    /**
     * Netty does not allow parameters to be set for non-POST/PUT requests
     */
    def setParameters(requestBuilder: AsyncHttpClient#BoundRequestBuilder): AsyncHttpClient#BoundRequestBuilder = {
        requestBuilder.setParameters(request.parameters.foldLeft(new FluentStringsMap()) { (fsMap, pair) =>
          fsMap.add(pair._1.toString, pair._2)
        })
    }

    import blueeyes.util.QueryParser
    import java.net.URI
    import scala.collection.mutable.LinkedHashMap

    // Merge request.parameters and original query params (in uri)
    val origURI = URI.create(request.uri)
    val newQueryParams = QueryParser.unparseQuery(request.parameters ++ QueryParser.parseQuery(Option(origURI.getRawQuery).getOrElse("")))
    val uri = new URI(origURI.getScheme, origURI.getAuthority, origURI.getPath, newQueryParams, origURI.getFragment).toString
      
    var requestBuilder = request.method match {
      case HttpMethods.CONNECT => Some(new AsyncHttpClient().prepareConnect(uri))
      case HttpMethods.DELETE => Some(new AsyncHttpClient().prepareDelete(uri))
      case HttpMethods.GET => Some(new AsyncHttpClient().prepareGet(uri))
      case HttpMethods.HEAD => Some(new AsyncHttpClient().prepareHead(uri))
      case HttpMethods.OPTIONS => Some(new AsyncHttpClient().prepareOptions(uri))
      case HttpMethods.POST => Some(setBody(new AsyncHttpClient().preparePost(uri)))
      case HttpMethods.PUT => Some(setBody(new AsyncHttpClient().preparePut(uri)))
      // TODO - CUSTOM
      // TODO - PATCH
      // TODO - TRACE
      case _ => None
    }

    val contentType: (String, String) = `Content-Type`(
        (for (`Content-Type`(contentType) <- request.headers) yield contentType).headOption.getOrElse(mimeType).asInstanceOf[MimeType])
    val contentLength: (String, String) = `Content-Length`(
        (for (`Content-Length`(contentLength) <- request.headers) yield contentLength).headOption.getOrElse(
            transcode(request.content.getOrElse(transcode.unapply(""))).length))
         
    val newHeaders = request.headers ++ Map(
        contentType,
        contentLength
    )

    for (pair <- newHeaders; r <- requestBuilder)
      yield r.setHeader(pair._1, pair._2)

    requestBuilder
  }
}

