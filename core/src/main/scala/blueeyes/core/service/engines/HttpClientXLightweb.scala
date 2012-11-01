package blueeyes.core.service
package engines

import blueeyes.bkka._
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.data.ByteChunk
import blueeyes.core.service.HttpClientByteChunk
import blueeyes.core.http.HttpStatusCodeImplicits._
import blueeyes.core.http.HttpFailure

import akka.dispatch.Promise
import akka.dispatch.Future
import akka.dispatch.ExecutionContext

import com.weiglewilczek.slf4s.Logging

import org.xlightweb.client.{HttpClient => XLHttpClient}
import org.xlightweb.{
  BodyDataSink,
  HttpRequestHeader,
  IHttpRequest,
  IHttpRequestHeader,
  IHeader,
  IHttpResponse,
  IHttpResponseHandler,
  DeleteRequest,
  GetRequest,
  HeadRequest,
  OptionsRequest,
  PostRequest,
  PutRequest,
  NonBlockingBodyDataSource,
  IBodyDataHandler,
  HttpRequest => XLHttpRequest
}

import java.io.IOException
import java.nio.ByteBuffer
import java.util.concurrent.locks.ReentrantReadWriteLock
import javax.net.ssl.SSLContext

import scalaz._
import scala.collection.JavaConverters._

class HttpClientXLightWeb(implicit val executor: ExecutionContext) extends HttpClientByteChunk with Logging {
  implicit val M: Monad[Future] = new FutureMonad(executor)

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
    val result = Promise[HttpResponse[ByteChunk]]()
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

  private def executeRequest(request: HttpRequest[ByteChunk], xlrequest: IHeader, promise: Promise[HttpResponse[ByteChunk]]) = {
    val clientInstance = httpClientInstance(request.uri.scheme)
    clientInstance.setAutoHandleCookies(false)

    val handler = new IHttpResponseHandler() {
      def onResponse(response: IHttpResponse) {
        if(response.getStatus >= 400) {
          val statusCode: HttpStatusCode = response.getStatus
          val reason: String = Option(response.getReason).getOrElse("Unknown error")

          statusCode match {
            case failureCode: HttpFailure => promise.failure(HttpException(failureCode, reason))
            case _ => promise.failure(HttpException(HttpStatusCodes.BadRequest, reason))
          }
        } else {
          val headers = response.getHeaderNameSet.asScala.foldLeft(Map[String, String]()) { 
            (acc, name) => acc + (name -> response.getHeader(name))
          }

          val isChunked = headers.exists {
            case (key, value) => 
              key.equalsIgnoreCase("Transfer-Encoding") && value.equalsIgnoreCase("chunked")
          }

          val data: Option[ByteChunk] = if (isChunked) readChunked(response) else readNotChunked(response)
          promise.success(HttpResponse[ByteChunk](status = HttpStatus(response.getStatus), content = data, headers = headers))
        }
      }

      def onException(e: IOException) {
        val httpStatus = e match {
          case _:java.net.ConnectException => HttpStatusCodes.ServiceUnavailable
          case _:java.net.SocketTimeoutException => HttpStatusCodes.ServiceUnavailable
          case _ => HttpStatusCodes.InternalServerError
        }

        promise.failure(HttpException(httpStatus, e))
      }
    }

    xlrequest match {
      case e: IHttpRequest => 
        clientInstance.send(e, handler)

      case e: IHttpRequestHeader =>
        val bodyDataSink = clientInstance.send(e, handler)
        request.content.map(sendData(_, bodyDataSink)).getOrElse(bodyDataSink.close())

      case r => sys.error("Unrecognized request type: " + r)
    }
  }

  private def sendData(chunk: ByteChunk, bodyDataSink: BodyDataSink) = {
    def writeStream(stream: StreamT[Future, ByteBuffer]): Future[Unit] = {
      stream.uncons flatMap {
        case Some((data, tail)) => 
          bodyDataSink.write(data)
          writeStream(tail)
        case None =>
          Future(bodyDataSink.close())
      }
    }

    chunk match {
      case Left(data) => Future(bodyDataSink.write(data)).map(_ => bodyDataSink.close())
      case Right(stream) => writeStream(stream)
    }
  }

  private def readNotChunked(response: IHttpResponse): Option[ByteChunk] = {
    val bytes = response.getBody.readBytes
    if (!bytes.isEmpty) Some(Left(ByteBuffer.wrap(bytes))) else None
  }

  private def readChunked(response: IHttpResponse): Option[ByteChunk] = {
    val head: Chain = Chain.incomplete

    response.getNonBlockingBody().setDataHandler {
      new IBodyDataHandler {
        private var chain: Chain = head

        override def onData(source: NonBlockingBodyDataSource) = {
          val available = source.available()
          if (available > 0) {
            val buffer = ByteBuffer.allocate(available)
            source.read(buffer)
            buffer.flip()

            val current = chain
            chain = Chain.incomplete
            current.promise.success(Some((buffer, chain)))
          } else if (available == -1){
            chain.promise.success(None)
          }

          true
        }
      }
    }

    Some(Right(StreamT.unfoldM[Future, ByteBuffer, Chain](head) { _.promise }))
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

    val newHeaders  = request.headers ++ requestContentLength(request)
    val xlRequest   = request.method match {
      case HttpMethods.DELETE     => new DeleteRequest(uri)
      case HttpMethods.GET        => new GetRequest(uri)
      case HttpMethods.HEAD       => new HeadRequest(uri)
      case HttpMethods.OPTIONS    => new OptionsRequest(uri)
      case HttpMethods.POST       => postRequest(request, uri)
      case HttpMethods.PUT        => putRequest(request, uri)
      case HttpMethods.CONNECT    => sys.error("CONNECT is not implemented.")
      case HttpMethods.TRACE      => sys.error("TRACE is not implemented.")
      case HttpMethods.PATCH      => sys.error("PATCH is not implemented.")
      case HttpMethods.CUSTOM(x)  => sys.error("CUSTOM is not implemented.")
    }

    for ((key, value) <- newHeaders.raw) xlRequest.addHeader(key, value)

    xlRequest
  }

  def isDefinedAt(request: HttpRequest[ByteChunk]) = {
    import HttpMethods._
    request.method match {
      case DELETE | GET | HEAD | OPTIONS | POST | PUT => true
      case _ => false
    }
  }

  private def postRequest(request: HttpRequest[ByteChunk], url: String): IHeader = {
    request.content match {
      case None => new PostRequest(url)
      case Some(Left(data))    => new PostRequest(url, requestContentType(request).value, Array(data))
      case Some(Right(stream)) => new HttpRequestHeader("POST", url, requestContentType(request).value)
    }
  }

  private def putRequest(request: HttpRequest[ByteChunk], url: String): IHeader = {
    request.content match{
      case None => new PutRequest(url)
      case Some(Left(data))    => new PutRequest(url, requestContentType(request).value, Array(data))
      case Some(Right(stream)) => new HttpRequestHeader("PUT", url, requestContentType(request).value)
    }
  }

  private def requestContentType(request: HttpRequest[ByteChunk]) = `Content-Type`(request.mimeTypes : _*)

  private def requestContentLength(request: HttpRequest[ByteChunk]): Option[HttpHeader] = {
    for (content <- request.content; v <- content.left.toOption) yield {
      `Content-Length`(v.remaining)
    }
  }
}
