package blueeyes.core.service.engines

import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.data.{ByteMemoryChunk, ByteChunk}
import blueeyes.core.service.HttpClient
import blueeyes.core.http.HttpStatusCodeImplicits._
import blueeyes.core.http.HttpFailure
import java.io.IOException
import java.util.concurrent.locks.ReentrantReadWriteLock
import javax.net.ssl.SSLContext
import net.lag.logging.Logger
import org.xlightweb.client.{HttpClient => XLHttpClient}
import org.xlightweb.{HttpRequest => XLHttpRequest, IHttpResponse, IHttpResponseHandler, DeleteRequest, GetRequest, HeadRequest,
                      OptionsRequest, PostRequest, PutRequest}
import scala.collection.JavaConversions._
import blueeyes.concurrent.{FutureDeliveryStrategySequential, Future}
import collection.mutable.ArrayBuilder.ofByte


trait HttpClientXLightWebEngines extends HttpClient[ByteChunk] with FutureDeliveryStrategySequential{

  protected def createSSLContext: SSLContext = SSLContext.getDefault()

  private var _httpClient: Option[XLHttpClient] = None
  private val lock = new ReentrantReadWriteLock

  private def httpClient( fn: () => XLHttpClient): XLHttpClient = _httpClient match {
    case None =>
      lock.writeLock.lock()
      try {
        _httpClient match {
          case None => {
            _httpClient = Some(fn())
            _httpClient.get
          }
          case Some(client) => client
        }
      }
      finally lock.writeLock.unlock()

    case Some(client) => client
  }

  def apply(request: HttpRequest[ByteChunk]): Future[HttpResponse[ByteChunk]] = {
    val result = new Future[HttpResponse[ByteChunk]]()
    executeRequest(request, result)
    result
  }

  private def executeRequest(request: HttpRequest[ByteChunk], resultFuture: Future[HttpResponse[ByteChunk]]) {
    val httpClientInstance = httpClient(() => {
      request.uri.scheme match{
        case Some("https") => new XLHttpClient(createSSLContext)
        case _ => new XLHttpClient()
      }
    })
    httpClientInstance.setAutoHandleCookies(false)

    httpClientInstance.send(createXLRequest(request), new IHttpResponseHandler() {
      def onResponse(response: IHttpResponse) {
        if(response.getStatus >= 400) {
          val statusCode: HttpStatusCode = response.getStatus
          val reason: String = Option(response.getReason).getOrElse("Unknown error")

          statusCode match {
            case failureCode: HttpFailure => resultFuture.cancel(HttpException(failureCode, reason))
            case _ => resultFuture.cancel(HttpException(HttpStatusCodes.BadRequest, reason))
          }
        } else {

          val data = try {
            val bytes = response.getBody.readBytes
            if (!bytes.isEmpty) Some(new ByteMemoryChunk(bytes)) else None
          } catch {
            case e: Throwable => {
              Logger.get.error(e, "Failed to transcode response body")
              None
            }
          }

          val headers = response.getHeaderNameSet.toList.asInstanceOf[List[String]].foldLeft(Map[String, String]()) { (acc: Map[String, String], name: String) =>
            acc + (name -> response.getHeader(name))
          }

          resultFuture.deliver(HttpResponse[ByteChunk](status = HttpStatus(response.getStatus), content = data, headers = headers))
        }
      }

      def onException(e: IOException) {
        val httpStatus = e match {
          case _:java.net.ConnectException => HttpStatusCodes.ServiceUnavailable
          case _:java.net.SocketTimeoutException => HttpStatusCodes.ServiceUnavailable
          case _ => HttpStatusCodes.InternalServerError
        }

        resultFuture.cancel(HttpException(httpStatus, e))
      }
    })
  }

  private def createXLRequest(request: HttpRequest[ByteChunk]): XLHttpRequest =  {
    import blueeyes.util.QueryParser
    import java.net.URI

    // Merge request.parameters and original query params (in uri)
    val origURI = new URI(request.uri.toString)
    val newQueryParams = QueryParser.unparseQuery(request.parameters ++ QueryParser.parseQuery(Option(origURI.getRawQuery).getOrElse("")), false)
    // URI expects nulls for undefined params, hence the conditional for the uri param
    val uri = new URI(origURI.getScheme, origURI.getAuthority, origURI.getPath,
                      if(newQueryParams.length == 0) null else newQueryParams,
                      origURI.getFragment).toString

    val byteContent = request.content.map(readContent(_))
    val newHeaders  = request.headers + requestContentLength(byteContent)
    val xlRequest   = createXLRequest(request, byteContent, uri)

    for (pair <- newHeaders)
      yield xlRequest.addHeader(pair._1, pair._2)

    xlRequest
  }

  def isDefinedAt(request: HttpRequest[ByteChunk]) = request.method match{
    case HttpMethods.DELETE | HttpMethods.GET | HttpMethods.HEAD | HttpMethods.OPTIONS | HttpMethods.POST | HttpMethods.PUT => true
    case _ => false
  }

  private def createXLRequest(request: HttpRequest[ByteChunk], content: Option[Array[Byte]], url: String): XLHttpRequest = {
    request.method match {
      case HttpMethods.DELETE     => new DeleteRequest(url)
      case HttpMethods.GET        => new GetRequest(url)
      case HttpMethods.HEAD       => new HeadRequest(url)
      case HttpMethods.OPTIONS    => new OptionsRequest(url)
      case HttpMethods.POST       => postRequest(request, content, url)
      case HttpMethods.PUT        => putRequest(request, content, url)
      case HttpMethods.CONNECT    => error("CONNECT is not implemented.")
      case HttpMethods.TRACE      => error("TRACE is not implemented.")
      case HttpMethods.PATCH      => error("PATCH is not implemented.")
      case HttpMethods.CUSTOM(x)  => error("CUSTOM is not implemented.")
    }
  }

  private def postRequest(request: HttpRequest[ByteChunk], content: Option[Array[Byte]], url: String) = {
    content.map(v => new PostRequest(url, requestContentType(request).value, v)).getOrElse[XLHttpRequest](new PostRequest(url))
  }
  private def putRequest(request: HttpRequest[ByteChunk], content: Option[Array[Byte]], url: String) = {
    content.map(v => new PutRequest(url, requestContentType(request).value, v)).getOrElse[XLHttpRequest](new PutRequest(url))
  }

  private def requestContentType(request: HttpRequest[ByteChunk]) = {
    val mimeType: List[MimeType] = (for (`Content-Type`(mimeTypes) <- request.headers) yield mimeTypes.toList).toList.flatten
    `Content-Type`(mimeType : _*)
  }
  private def requestContentLength(content: Option[Array[Byte]]) = `Content-Length`(content.map(_.size).getOrElse[Int](0))

  private def readContent(chunk: ByteChunk): Array[Byte] = readContent(chunk, new ofByte()).result
  private def readContent(chunk: ByteChunk, buffer: ofByte): ofByte = {
    buffer ++= chunk.data

    val next = chunk.next
    next match{
      case None =>  buffer
      case Some(x) => readContent(x.value.get, buffer)
    }
  }
}