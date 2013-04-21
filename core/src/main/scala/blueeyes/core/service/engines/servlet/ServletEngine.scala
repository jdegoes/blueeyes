package blueeyes.core.service
package engines.servlet

import blueeyes.bkka._
import blueeyes.core.data._
import blueeyes.core.http.HttpResponse

import akka.dispatch.{Promise, Future, ExecutionContext, Await}
import akka.util.Timeout
import akka.util.FiniteDuration

import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.channels._
import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import javax.servlet.AsyncContext
import java.util.concurrent.CountDownLatch

import org.streum.configrity.Configuration
import org.streum.configrity.io.BlockFormat

import scalaz._
import scala.collection.mutable.{HashSet, SynchronizedSet}

abstract class ServletEngine extends HttpServlet with HttpServerModule with HttpServletConverters {
  private var service: AsyncHttpService[ByteChunk] = null
  private var stopTimeout: Timeout = null
  private var stoppable: Option[Stoppable] = null
  private var _executionContext: ExecutionContext = null

  private val pendingResponses = new HashSet[Future[HttpResponse[ByteChunk]]] with SynchronizedSet[Future[HttpResponse[ByteChunk]]]

  override def service(httpRequest: HttpServletRequest, httpResponse: HttpServletResponse) {
    implicit val executor: ExecutionContext = _executionContext
    implicit val M: Monad[Future] = new FutureMonad(executor)

    def writeResponse(response: HttpResponse[ByteChunk], asyncContext: AsyncContext): Future[Unit] = {
      def writeStream(stream: StreamT[Future, ByteBuffer], channel: WritableByteChannel): Future[Unit] = {
        stream.uncons flatMap {
          case Some((head, tail)) =>
            channel.write(head)
            writeStream(tail, channel)
          case None =>
            Future {
              channel.close()
              asyncContext.complete()
            }
        }
      }

      val realHttpResponse = asyncContext.getResponse.asInstanceOf[HttpServletResponse]

      response.headers.raw.foreach(header => realHttpResponse.setHeader(header._1, header._2))
      if (response.status.code.value >= 400){
        Future {
          realHttpResponse.sendError(response.status.code.value, response.status.code.reason)
        }
      } else {
        realHttpResponse.setStatus(response.status.code.value)
        response.content match {
          case Some(Left(buffer)) => 
            realHttpResponse.setHeader("Content-Length", buffer.remaining.toString)

            Future {  
              val channel = Channels.newChannel(realHttpResponse.getOutputStream)
              channel.write(buffer)
              channel.close()
              asyncContext.complete()
            }

          case Some(Right(stream)) => 
            realHttpResponse.setHeader("Transfer-Encoding", "chunked")
            writeStream(stream, Channels.newChannel(realHttpResponse.getOutputStream))

          case None =>
            Future { asyncContext.complete() }
        }
      }
    }

    for (responseFuture <- service.service(fromServletRequest(httpRequest))) {
      pendingResponses += responseFuture
      for (response <- responseFuture; _ <- writeResponse(response, httpRequest.startAsync(httpRequest, httpResponse))) {
        pendingResponses -= responseFuture
      }
    }
  }

  override def destroy() {
    implicit val executor = _executionContext
    pendingResponses.foreach(_.asInstanceOf[Promise[HttpResponse[ByteChunk]]].failure(new RuntimeException("Connection closed.")))
    pendingResponses.clear()

    stoppable foreach { s =>
      Await.result(Stoppable.stop(s), stopTimeout.duration);
    }
  }

  override def init() {
    val rootConfiguration = Configuration.load(
      Option(getInitParameter("configFile")).getOrElse(sys.error("Expected --configFile init parameter")),
      BlockFormat
    )

    _executionContext = AkkaDefaults.defaultFutureDispatch
    val run = server(rootConfiguration, _executionContext)
    run.start map { startFuture => 
      val startupTimeout = Timeout(rootConfiguration[Long]("server.startup.timeout.seconds", 30L))
      val started = startFuture map {
        case (svc, stop) => 
          service = svc
          stoppable = stop
          stopTimeout = run.config.stopTimeout
      }

      Await.result(started, startupTimeout.duration)
    } getOrElse {
      sys.error("Unable to start service; no BlueEyes services available.")
    }
  }
}
