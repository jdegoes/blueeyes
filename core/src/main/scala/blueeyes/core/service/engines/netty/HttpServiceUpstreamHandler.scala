package blueeyes.core.service
package engines.netty

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext

import blueeyes.bkka._
import blueeyes.concurrent.ReadWriteLock
import blueeyes.core.http._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.HttpHeaders.{`Content-Type`}
import blueeyes.core.http.HttpStatusCodes.{InternalServerError, NotFound}
import blueeyes.core.data._
import blueeyes.util._

import com.weiglewilczek.slf4s.Logger
import com.weiglewilczek.slf4s.Logging

import java.io.IOException

import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.channel.{
  SimpleChannelUpstreamHandler,
  MessageEvent,
  Channel,
  ChannelFutureListener,
  ChannelHandler,
  ChannelHandlerContext,
  ChannelStateEvent,
  ExceptionEvent
}
import org.jboss.netty.handler.codec.http.{
  QueryStringDecoder,
  DefaultHttpChunk,
  DefaultHttpChunkTrailer,
  DefaultHttpResponse,
  HttpChunk,
  HttpMethod => NettyHttpMethod,
  HttpVersion => NettyHttpVersion,
  HttpHeaders => NettyHttpHeaders,
  HttpRequest => NettyHttpRequest,
  HttpResponse => NettyHttpResponse,
  HttpResponseStatus
}
import org.jboss.netty.handler.codec.http.HttpHeaders._
import org.jboss.netty.handler.stream.ChunkedInput
import org.jboss.netty.handler.stream.ChunkedWriteHandler

import java.nio.ByteBuffer
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue

import scalaz._
import scalaz.syntax.monad._
import scalaz.syntax.show._
import scala.collection.JavaConverters._
import scala.collection.mutable.{HashSet, SynchronizedSet}

/** This handler is not thread safe, it's assumed a new one will be created
 * for each client connection.
 *
 * TODO: Pass health monitor to the request handler to report on Netty errors.
 */
private[engines] class HttpServiceUpstreamHandler(service: AsyncHttpService[ByteChunk, ByteChunk], executionContext: ExecutionContext) extends SimpleChannelUpstreamHandler with Logging {
  private val pendingResponses = new HashSet[Future[HttpResponse[ByteChunk]]] with SynchronizedSet[Future[HttpResponse[ByteChunk]]]
  private implicit val M: Monad[Future] = new FutureMonad(executionContext)

  private def textResponse(status0: HttpStatus, detail: Option[String]) = HttpResponse[ByteChunk](
    status = status0,
    headers = HttpHeaders(`Content-Type`(text/plain)),
    content = detail map { s => Left(s.getBytes("UTF-8")) }
  )

  override def messageReceived(ctx: ChannelHandlerContext, event: MessageEvent) {
    val request = event.getMessage.asInstanceOf[HttpRequest[ByteChunk]]
    ctx.setAttachment(request)
    service.service(request) match {
      case Success(responseFuture) =>
        pendingResponses += responseFuture

        for (response <- responseFuture) {
          pendingResponses -= responseFuture
          writeResponse(request, event.getChannel, response)
        }

      case Failure(DispatchError(httpFailure, message, detail)) =>
        writeResponse(request, ctx.getChannel,  textResponse(HttpStatus(httpFailure, message), detail))

      case Failure(Inapplicable(_)) =>
        writeResponse(request, ctx.getChannel, textResponse(NotFound, Some("No service was found to be able to handle your request: %s".format(request.shows))))
    }
  }

  override def channelClosed(ctx: ChannelHandlerContext, e: ChannelStateEvent) = {
    super.channelClosed(ctx, e)

    killPending(new RuntimeException("Channel closed."))
  }

  override def channelDisconnected(ctx: ChannelHandlerContext, e: ChannelStateEvent) = {
    super.channelDisconnected(ctx, e)

    killPending(new RuntimeException("Channel disconnected."))
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {
    try {
      killPending(e.getCause)

      val request = ctx.getAttachment.asInstanceOf[HttpRequest[ByteChunk]]

      e.getCause match {
        case ex @ HttpException(code, reason) =>
          logger.warn("An exception was raised by an I/O thread or a ChannelHandler", ex)
          writeResponse(request, ctx.getChannel, textResponse(code, Option(reason)))

        case ioe: IOException if Option(ioe.getMessage).exists(_.contains("reset by peer")) =>
          try {
            logger.warn("Connection reset by peer: " + Option(ctx.getChannel.getRemoteAddress).getOrElse("unknown"))
            //ctx.getChannel.close() // this didn't work, caused errors draining streams, why?
          } catch {
            case ex: Exception => logger.error("Error thrown attempting to get channel remote address", ex)
          }

        case ex =>
          // here we do not want to 
          logger.error("An exception was raised by an I/O thread or a ChannelHandler", ex)
          writeResponse(request, ctx.getChannel, textResponse(InternalServerError, None))
      }
    } catch {
      case ex =>
        logger.error("An exception was caught attempting to handle an exception in the Netty exceptionCaught handler.", ex)
        super.exceptionCaught(ctx, e)
    }
  }

  private def writeResponse(request: HttpRequest[ByteChunk], channel: Channel, response: HttpResponse[ByteChunk]) = {
    import HttpNettyConverters._

    val nettyResponse = new DefaultHttpResponse(toNettyVersion(response.version), toNettyStatus(response.status))
    for (header <- response.headers.raw) nettyResponse.setHeader(header._1, header._2)

    if (channel.isConnected) {
      val nettyFuture = response.content match {
        case Some(Left(bytes)) =>
          nettyResponse.setHeader(NettyHttpHeaders.Names.CONTENT_LENGTH, bytes.length.toString)
          nettyResponse.setContent(ChannelBuffers.copiedBuffer(ByteBuffer.wrap(bytes)))
          channel.write(nettyResponse)

        case Some(Right(stream)) =>
          nettyResponse.setHeader(NettyHttpHeaders.Names.TRANSFER_ENCODING, "chunked")
          channel.write(nettyResponse)
          channel.write(StreamChunkedInput(stream.map(ByteBuffer.wrap(_)), channel, 2))

        case None =>
          nettyResponse.setHeader(NettyHttpHeaders.Names.CONTENT_LENGTH, "0")
          channel.write(nettyResponse)
      }

      if (request != null && !isKeepAlive(request)) nettyFuture.addListener(ChannelFutureListener.CLOSE)
    }
  }

  private def isKeepAlive(message: HttpRequest[ByteChunk] ): Boolean = {
    val connection = message.headers.get(Names.CONNECTION).getOrElse("")
    if (connection.equalsIgnoreCase(Values.CLOSE)) false
    else {
      message.version match {
        case HttpVersions.`HTTP/1.0` => Values.KEEP_ALIVE.equalsIgnoreCase(connection)
        case HttpVersions.`HTTP/1.1` => !Values.CLOSE.equalsIgnoreCase(connection)
      }
    }
  }

  private def killPending(why: Throwable) {
    // Kill all pending responses to this channel:
    pendingResponses.foreach {
      case (pr: Promise[_]) => pr.tryComplete(Left(why))
      case notPromise => logger.error("Pending response was not a promise, but a %s. This should never happen.".format(Option(notPromise).toString))
    }
    pendingResponses.clear()
  }
}

private[engines] class StreamChunkedInput(queue: BlockingQueue[Option[HttpChunk]], channel: Channel) extends ChunkedInput {
  override def hasNextChunk() = {
    val head = queue.peek
    (head != None && head != null)
  }

  override def nextChunk() = {
    queue.poll() match {
      case None | null => null
      case Some(data) => data
    }
  }

  override def isEndOfInput() = {
    queue.peek == None
  }

  override def close() = ()
}

object StreamChunkedInput extends Logging {
  def apply(stream: StreamT[Future, ByteBuffer], channel: Channel, maxQueueSize: Int = 1)(implicit M: Monad[Future]): ChunkedInput = {
    def advance(queue: BlockingQueue[Option[HttpChunk]], stream: StreamT[Future, ByteBuffer]): Future[Unit] = {
      stream.uncons flatMap {
        case Some((buffer, tail)) =>
          queue.put(Some(new DefaultHttpChunk(ChannelBuffers.wrappedBuffer(buffer))))
          channel.getPipeline.get(classOf[ChunkedWriteHandler]).resumeTransfer()
          advance(queue, tail)

        case None =>
          {
            queue.put(Some(new DefaultHttpChunkTrailer))
            queue.put(None)
            channel.getPipeline.get(classOf[ChunkedWriteHandler]).resumeTransfer()
          }.point[Future]
      } recover {
        case ex =>
          logger.error("An error was encountered retrieving the next chunk of data: " + ex.getMessage, ex)
          queue.put(None)
          channel.getPipeline.get(classOf[ChunkedWriteHandler]).resumeTransfer()
      }
    }

    val queue = new LinkedBlockingQueue[Option[HttpChunk]](maxQueueSize)
    advance(queue, stream).onSuccess {
      case _ => logger.debug("Response stream fully consumed by Netty.")
    }

    new StreamChunkedInput(queue, channel)
  }
}
