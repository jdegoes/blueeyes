package blueeyes.core.service.engines

import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpStatusCodeImplicits._
import blueeyes.core.http.HttpHeaderImplicits._
import blueeyes.core.http.HttpNumberImplicits._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http._
import blueeyes.core.service.HttpClient
import blueeyes.core.data.Bijection
import blueeyes.util.Future
import java.io.IOException
import java.util.concurrent.{ Future => JavaFuture }
import org.xlightweb.{ 
  HttpRequest => XLHttpRequest, 
  IHttpResponse, IHttpResponseHandler, 
  DeleteRequest, GetRequest, HeadRequest, OptionsRequest, PostRequest, PutRequest }
import org.xlightweb.client.{HttpClient => XLHttpClient}

trait HttpClientXLightWeb[T] extends HttpClient[T] {
  def transcode: Bijection[T, String]
  def mimeType: MimeType 

  def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = {
    new Future[HttpResponse[T]]() {
      executeRequest(request, new IHttpResponseHandler() {
        def onResponse(response: IHttpResponse) {
          val data = try {
            Some(transcode.unapply(response.getBody.readString))
          } catch {
            // Need to log here...
            case _ => None
          }
          deliver(HttpResponse[T](status = HttpStatus(response.getStatus), content = data));
        }

        def onException(e: IOException) {
          val httpStatus = HttpStatus(e match {
            case _:java.net.ConnectException => HttpStatusCodes.ServiceUnavailable
            case _ => HttpStatusCodes.InternalServerError
          })
          cancel(new Error(e))
        }
      })
    }
  }

  /**
   * Creates an HttpClient using XlightWeb http client
   * @param request
   * @return
   */
  protected def executeRequest(request: HttpRequest[T], responseHandler: IHttpResponseHandler) {
    val httpClient = new XLHttpClient()

    def getBody(): Option[String] = {
      request.content.map(transcode(_))
    }

    def createXLRequest(url: String, contentType: HttpHeader): Option[XLHttpRequest] = {
      request.method match {
        case HttpMethods.DELETE => Some(new DeleteRequest(url))
        case HttpMethods.GET => Some(new GetRequest(url))
        case HttpMethods.HEAD => Some(new HeadRequest(url))
        case HttpMethods.OPTIONS => Some(new OptionsRequest(url))
        case HttpMethods.POST => Some(getBody.map(new PostRequest(url, contentType.value, _)).getOrElse[XLHttpRequest](new PostRequest(url)))
        case HttpMethods.PUT => Some(getBody.map(new PutRequest(url, contentType.value, _)).getOrElse[XLHttpRequest](new PutRequest(url)))
        // TODO - CONNECT 
        // TODO - CUSTOM
        // TODO - PATCH
        // TODO - TRACE
        case _ => None
      }
    }

    import blueeyes.util.QueryParser
    import java.net.URI
    import scala.collection.mutable.LinkedHashMap

    // Merge request.parameters and original query params (in uri)
    val origURI = URI.create(request.uri)
    val newQueryParams = QueryParser.unparseQuery(request.parameters ++ QueryParser.parseQuery(Option(origURI.getRawQuery).getOrElse("")))
    // URI expects nulls for undefined params, hence the conditional for the uri param
    val uri = new URI(origURI.getScheme, origURI.getAuthority, origURI.getPath, 
                      if(newQueryParams.length == 0) null else newQueryParams, 
                      origURI.getFragment).toString

    val contentType: (String, String) = `Content-Type`(
      (for (`Content-Type`(contentType) <- request.headers) yield contentType.apply(0)).headOption.getOrElse[MimeType](mimeType))

    val contentLength: (String, String) = `Content-Length`(request.content.map(c => transcode(c).length).getOrElse[Int](0))

    val newHeaders = request.headers ++ Map(
        contentType,
        contentLength
    )

    val req = createXLRequest(uri, contentType)

    for (pair <- newHeaders; r <- req) 
      yield r.addHeader(pair._1, pair._2)

    // Execute the HTTP request
    req.foreach(httpClient.send(_, responseHandler))
  }
}

