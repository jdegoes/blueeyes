package blueeyes.core.service.engines.servlet

import blueeyes.core.service.{HttpServer, HttpServerEngine}
import java.util.concurrent.CountDownLatch
import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import blueeyes.core.http.HttpResponse
import collection.mutable.{HashSet, SynchronizedSet}
import javax.servlet.AsyncContext
import blueeyes.core.data.{MemoryChunk, ByteChunk}
import java.io.OutputStream
import net.lag.configgy.Configgy
import akka.dispatch.{Promise, Future}

trait ServletEngine extends HttpServlet with HttpServerEngine with HttpServer with HttpServletConverters{
  private val pendingResponses = new HashSet[Future[HttpResponse[ByteChunk]]] with SynchronizedSet[Future[HttpResponse[ByteChunk]]]

  override def service(httpRequest: HttpServletRequest, httpResponse: HttpServletResponse) {
    def writeResponse(response: HttpResponse[ByteChunk], asyncContext: AsyncContext) {
      val chunkedContent   = new ChunkedContent(response.content)
      val realHttpResponse = asyncContext.getResponse.asInstanceOf[HttpServletResponse]

      response.headers.raw.foreach(header => realHttpResponse.setHeader(header._1, header._2))
      if (response.status.code.value >= 400){
        realHttpResponse.sendError(response.status.code.value, response.status.code.reason)
      }
      else{
        val (header, value) = if (chunkedContent.isChunked) ("Transfer-Encoding", "chunked") else ("Content-Length", response.content.map(_.data.length.toString).getOrElse("0"))
        realHttpResponse.setHeader(header, value)
        realHttpResponse.setStatus(response.status.code.value)
      }

      chunkedContent.chunk match{
        case Some(e) => writeContent(e, realHttpResponse.getOutputStream, asyncContext)
        case None    => asyncContext.complete()
      }
    }

    service(fromServletRequest(httpRequest)).foreach {responseFuture =>
      pendingResponses += responseFuture
      val asyncContext = httpRequest.startAsync(httpRequest, httpResponse);
      responseFuture foreach { response =>
        pendingResponses -= responseFuture
        writeResponse(response, asyncContext)
      }
    }
  }

  private def writeContent(content: ByteChunk, outputStream: OutputStream, asyncContext: AsyncContext){
    outputStream.write(content.data)
    content.next match{
      case None => asyncContext.complete()
      case Some(future) =>
        future onSuccess {case nextChunk => writeContent(nextChunk, outputStream, asyncContext)}
        future onFailure {case error => asyncContext.complete()}
    }
  }

  override def destroy() {
    pendingResponses.foreach(_.asInstanceOf[Promise[HttpResponse[ByteChunk]]].failure(new RuntimeException("Connection closed.")))
    pendingResponses.clear()

    waitDone(stop);
  }

  override def init() {
    Configgy.configure(Option(getInitParameter("configFile")).getOrElse(sys.error("Expected --configFile init parameter")))
    waitDone(start)
  }

  private def waitDone(future: Future[_]){
    val doneSignal = new CountDownLatch(1)

    future.onSuccess { case _ =>
      doneSignal.countDown()
    }.onFailure { case e =>
      doneSignal.countDown()
    }

    doneSignal.await()
  }
}

private[engines] class ChunkedContent(content: Option[ByteChunk]){
  val (chunk, isChunked) = content map { value =>
    val nextChunk = value.next
    nextChunk match {
      case Some(next) => (Some(new MemoryChunk(value.data, () => {nextChunk})), true)
      case None       => (content, false)
    }
  } getOrElse ((None, false))
}