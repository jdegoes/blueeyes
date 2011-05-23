package blueeyes.core.service.engines

import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.data.{ByteMemoryChunk, ByteChunk}
import blueeyes.core.service.HttpClientByteChunk
import blueeyes.core.http.HttpStatusCodeImplicits._
import blueeyes.core.http.HttpFailure
import java.io.IOException
import java.util.concurrent.locks.ReentrantReadWriteLock
import javax.net.ssl.SSLContext
import net.lag.logging.Logger
import org.xlightweb.client.{HttpClient => XLHttpClient}
import scala.collection.JavaConversions._
import blueeyes.concurrent.{FutureDeliveryStrategySequential, Future}
import org.xlightweb.{BodyDataSink, HttpRequestHeader, IHttpRequest, IHttpRequestHeader, IHeader, IHttpResponse, IHttpResponseHandler, DeleteRequest, GetRequest, HeadRequest, OptionsRequest, PostRequest, PutRequest, NonBlockingBodyDataSource, IBodyDataHandler, HttpRequest => XLHttpRequest}

trait HttpClientXLightWebEngines extends HttpClientByteChunk with FutureDeliveryStrategySequential{

  private val logger = Logger.get

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
    executeRequest(request, createXLRequest(request), result)
    result
  }

  private def httpClientInstance(scheme: Option[String]) = {
    httpClient(() => {
      scheme match{
        case Some("https") => new XLHttpClient(createSSLContext)
        case _ => new XLHttpClient()
      }
    })
  }

  private def executeRequest(request: HttpRequest[ByteChunk], xlrequest: IHeader, resultFuture: Future[HttpResponse[ByteChunk]]) = {
    val clientInstance = httpClientInstance(request.uri.scheme)
    clientInstance.setAutoHandleCookies(false)
    val handler = new IHttpResponseHandler() {
      def onResponse(response: IHttpResponse) {
        if(response.getStatus >= 400) {
          val statusCode: HttpStatusCode = response.getStatus
          val reason: String = Option(response.getReason).getOrElse("Unknown error")

          statusCode match {
            case failureCode: HttpFailure => resultFuture.cancel(HttpException(failureCode, reason))
            case _ => resultFuture.cancel(HttpException(HttpStatusCodes.BadRequest, reason))
          }
        } else {

          val headers = response.getHeaderNameSet.toList.asInstanceOf[List[String]].foldLeft(Map[String, String]()) { (acc: Map[String, String], name: String) =>
            acc + (name -> response.getHeader(name))
          }

          val isChunked = headers.find((keyValue) => keyValue._1.equalsIgnoreCase("Transfer-Encoding") && keyValue._2.equalsIgnoreCase("chunked")).map(v => true).getOrElse(false)

          if (isChunked) readChunked(response, headers, resultFuture)
          else readNotChunked(response, headers, resultFuture)
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
    }

    xlrequest match {
      case e: IHttpRequest => clientInstance.send(e, handler)
      case e: IHttpRequestHeader =>
        val bodyDataSink = clientInstance.send(e, handler)
        request.content.map(sendData(_, bodyDataSink)).getOrElse(bodyDataSink.close())
      case _ => error("wrong request type")
    }
  }

  private def sendData(chunk: ByteChunk, bodyDataSink: BodyDataSink) {
    try {
      bodyDataSink.write(chunk.data, 0, chunk.data.length)

      chunk.next match {
        case Some(x) => x.deliverTo(nextChunk => sendData(nextChunk, bodyDataSink))
        case None =>
          bodyDataSink.close
      }
    }
    catch {
      case e: Throwable =>
        logger.error(e, "Failed to send content")
        bodyDataSink.close
    }
  }

  private def readNotChunked(response: IHttpResponse, headers: Map[String, String], resultFuture: Future[HttpResponse[ByteChunk]]){
    val data = try {
      val bytes = response.getBody.readBytes
      if (!bytes.isEmpty) Some(new ByteMemoryChunk(bytes)) else None
    } catch {
      case e: Throwable => {
        logger.error(e, "Failed to transcode response body")
        None
      }
    }
    resultFuture.deliver(HttpResponse[ByteChunk](status = HttpStatus(response.getStatus), content = data, headers = headers))
  }

  private def readChunked(response: IHttpResponse, headers: Map[String, String], resultFuture: Future[HttpResponse[ByteChunk]]){
    val dataSource = response.getNonBlockingBody()
    dataSource.setDataHandler(new IBodyDataHandler{
      var delivery: Either[HttpResponse[ByteChunk], Future[ByteChunk]] = Left(HttpResponse[ByteChunk](status = HttpStatus(response.getStatus), content = None, headers = headers))

      def onData(source: NonBlockingBodyDataSource) = {
        try {
          val available = source.available()
          if (available > 0) {
            val data        = org.xsocket.DataConverter.toBytes(source.readByteBufferByLength(available))
            val nextFuture  = new Future[ByteChunk]()
            val content     = new ByteMemoryChunk(data, () => Some(nextFuture))
            delivery match {
              case Left(x) =>
                delivery        = Right(nextFuture)
                resultFuture.deliver(x.copy(content = Some(content)))
              case Right(x) =>
                delivery        = Right(nextFuture)
                x.deliver(content)
            }
          }
          else if (available == -1){
            delivery match {
              case Left(x) => resultFuture.deliver(x)
              case Right(x) => x.deliver(new ByteMemoryChunk(Array[Byte]()))
            }
          }
        }
        catch {
          case e: Throwable =>
            delivery match {
              case Left(x) => resultFuture.cancel(e)
              case Right(x) => x.cancel(e)
            }
        }
        true
      }
    })
  }

  private def createXLRequest(request: HttpRequest[ByteChunk]): IHeader =  {
    import blueeyes.util.QueryParser
    import java.net.URI

    // Merge request.parameters and original query params (in uri)
    val origURI = new URI(request.uri.toString)
    val newQueryParams = QueryParser.unparseQuery(request.parameters ++ QueryParser.parseQuery(Option(origURI.getRawQuery).getOrElse("")), false)
    // URI expects nulls for undefined params, hence the conditional for the uri param
    val uri = new URI(origURI.getScheme, origURI.getAuthority, origURI.getPath,
                      if(newQueryParams.length == 0) null else newQueryParams,
                      origURI.getFragment).toString

    val newHeaders  = requestContentLength(request).foldLeft(request.headers){(headers, contentLength) => headers + contentLength}
    val xlRequest   = createXLRequest(request, uri)

    for (pair <- newHeaders)
      yield xlRequest.addHeader(pair._1, pair._2)

    xlRequest
  }

  def isDefinedAt(request: HttpRequest[ByteChunk]) = request.method match{
    case HttpMethods.DELETE | HttpMethods.GET | HttpMethods.HEAD | HttpMethods.OPTIONS | HttpMethods.POST | HttpMethods.PUT => true
    case _ => false
  }

  private def createXLRequest(request: HttpRequest[ByteChunk], url: String): IHeader = {
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

  private def postRequest(request: HttpRequest[ByteChunk], url: String): IHeader = {
    request.content match{
      case None => new PostRequest(url)
      case Some(x) if (x.next == None) => new PostRequest(url, requestContentType(request).value, x.data)
      case Some(x) => new HttpRequestHeader("POST", url, requestContentType(request).value)
    }
  }
  private def putRequest(request: HttpRequest[ByteChunk], url: String): IHeader = {
    request.content match{
      case None => new PutRequest(url)
      case Some(x) if (x.next == None) => new PutRequest(url, requestContentType(request).value, x.data)
      case Some(x) => new HttpRequestHeader("PUT", url, requestContentType(request).value)
    }
  }

  private def requestContentType(request: HttpRequest[ByteChunk]) = {
    val mimeType: List[MimeType] = (for (`Content-Type`(mimeTypes) <- request.headers) yield mimeTypes.toList).toList.flatten
    `Content-Type`(mimeType : _*)
  }
  private def requestContentLength(request: HttpRequest[ByteChunk]) = notChunkedContent(request.content).map(v => `Content-Length`(v.size))

  private def notChunkedContent(chunk: Option[ByteChunk]): Option[Array[Byte]] = {
    chunk.flatMap{value => value.next match {
        case Some(x) => None
        case None    => Some(value.data)
      }
    }
  }
}