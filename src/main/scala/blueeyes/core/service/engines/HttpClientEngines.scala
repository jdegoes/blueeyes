package blueeyes.core.service.engines

import javax.net.ssl.SSLContext
import java.io.IOException
import blueeyes.util.Future
import org.xlightweb.client.{HttpClient => XLHttpClient}
import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import org.xlightweb.{HttpRequest => XLHttpRequest, IHttpResponse, IHttpResponseHandler, DeleteRequest, GetRequest, HeadRequest,
                      OptionsRequest, PostRequest, PutRequest, BodyDataSource}
import blueeyes.core.data.Bijection
import blueeyes.core.service.HttpClient
import net.lag.logging.Logger


sealed trait HttpClientXLightWebEngines[T] extends HttpClient[T]{
  def contentBijection: Bijection[BodyDataSource, T]

  protected def createSSLContext: SSLContext = SSLContext.getDefault()

  def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = {
    val result = new Future[HttpResponse[T]]() 
    executeRequest(request, result)
    result
  }

  private def executeRequest(request: HttpRequest[T], resultFuture: Future[HttpResponse[T]]) {
    val httpClient = if (request.scheme == "https") new XLHttpClient(createSSLContext) else new XLHttpClient()

    httpClient.send(createXLRequest(request), new IHttpResponseHandler() {
      def onResponse(response: IHttpResponse) {
        val data = try {
          Some(contentBijection(response.getBody))
        } catch {
          case e: Throwable => {
	    Logger.get.error(e, "Failed to transcode response body")
	    None
	  }
        }
        
        resultFuture.deliver(HttpResponse[T](status = HttpStatus(response.getStatus), content = data))
        httpClient.close
      }

      def onException(e: IOException) {
        val httpStatus = HttpStatus(e match {
          case _:java.net.ConnectException => HttpStatusCodes.ServiceUnavailable
	  case _:java.net.SocketTimeoutException => HttpStatusCodes.ServiceUnavailable
          case _ => HttpStatusCodes.InternalServerError
        })

        httpClient.close
        resultFuture.deliver(HttpResponse[T](status = httpStatus))
      }
    })
  }
  private def createXLRequest(request: HttpRequest[T]): XLHttpRequest =  {
    import blueeyes.util.QueryParser
    import java.net.URI

    // Merge request.parameters and original query params (in uri)
    val origURI = URI.create(request.uri)
    val newQueryParams = QueryParser.unparseQuery(request.parameters ++ QueryParser.parseQuery(Option(origURI.getRawQuery).getOrElse("")))
    // URI expects nulls for undefined params, hence the conditional for the uri param
    val uri = new URI(origURI.getScheme, origURI.getAuthority, origURI.getPath,
                      if(newQueryParams.length == 0) null else newQueryParams,
                      origURI.getFragment).toString

    val newHeaders         = request.headers + requestContentLength(request)
    val xlRequest          = createXLRequest(request, uri)

    for (pair <- newHeaders)
      yield xlRequest.addHeader(pair._1, pair._2)

    xlRequest
  }

  private def createXLRequest(request: HttpRequest[T], url: String): XLHttpRequest = {
    request.method match {
      case HttpMethods.DELETE     => new DeleteRequest(url)
      case HttpMethods.GET        => new GetRequest(url)
      case HttpMethods.HEAD       => new HeadRequest(url)
      case HttpMethods.OPTIONS    => new OptionsRequest(url)
      case HttpMethods.POST       => postRequest(request, url)
      case HttpMethods.PUT        => putRequest(request, url)
      case HttpMethods.CONNECT    => error("CONNECT is not implemented.")
      case HttpMethods.TRACE      => error("TRACE is not implemented.")
      case HttpMethods.PATCH      => error("PATCH is not implemented.")
      case HttpMethods.CUSTOM(x)  => error("CUSTOM is not implemented.")
    }
  }

  private def postRequest(request: HttpRequest[T], url: String) = {
    request.content.map(v => {
      val contentType = requestContentType(request).value
      if (v.isInstanceOf[String])       new PostRequest(url, requestContentType(request).value, v.asInstanceOf[String])
      else if (v.isInstanceOf[Array[Byte]])  new PostRequest(url, requestContentType(request).value, v.asInstanceOf[String])
      else error("Unsupported body type. Content type can be either String or Array[Byte].")
    }).getOrElse[XLHttpRequest](new PostRequest(url))
  }
  private def putRequest(request: HttpRequest[T], url: String) = {
    request.content.map(v => {
      val contentType = requestContentType(request).value
      if (v.isInstanceOf[String])       new PutRequest(url, requestContentType(request).value, v.asInstanceOf[String])
      else if (v.isInstanceOf[Array[Byte]])  new PutRequest(url, requestContentType(request).value, v.asInstanceOf[String])
      else error("Unsupported content type. Content type can be either String or Array[Byte].")
    }).getOrElse[XLHttpRequest](new PutRequest(url))
  }

  private def requestContentType(request: HttpRequest[T]) = {
    val mimeType: List[MimeType] = (for (`Content-Type`(mimeTypes) <- request.headers) yield mimeTypes.toList).toList.flatten
    `Content-Type`(mimeType : _*)
  }
  private def requestContentLength(request: HttpRequest[T]) = {
    `Content-Length`(request.content.map(v => {
      if (v.isInstanceOf[String]) v.asInstanceOf[String].length
      else if (v.isInstanceOf[Array[Byte]]) v.asInstanceOf[Array[Byte]].size
      else error("Unsupported content type. Content type can be either String or Array[Byte].")
    }).getOrElse[Int](0))
  }
}

trait HttpClientXLightWebEnginesArrayByte extends HttpClientXLightWebEngines[Array[Byte]]{
  val contentBijection = XLightWebRequestBijections.BodyDataSourceToByteArray
}

trait HttpClientXLightWebEnginesString extends HttpClientXLightWebEngines[String]{
  val contentBijection = XLightWebRequestBijections.BodyDataSourceToString
}

object XLightWebRequestBijections{
  val BodyDataSourceToByteArray = new Bijection[BodyDataSource, Array[Byte]]{
    def apply(content: BodyDataSource) = content.readBytes
    def unapply(content: Array[Byte])  = error("Not imlemented")
  }

  val BodyDataSourceToString = new Bijection[BodyDataSource, String]{
    def apply(content: BodyDataSource) = content.readString
    def unapply(content: String)       = error("Not imlemented")
  }
}

