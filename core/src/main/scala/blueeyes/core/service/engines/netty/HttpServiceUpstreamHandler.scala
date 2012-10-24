package blueeyes.core.service
package engines.netty

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext

import blueeyes.bkka._
import blueeyes.concurrent.ReadWriteLock
import blueeyes.core.http._
import blueeyes.core.data._

import com.weiglewilczek.slf4s.Logger
import com.weiglewilczek.slf4s.Logging

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

import scala.collection.JavaConversions._
import scala.collection.mutable.{HashSet, SynchronizedSet}
import scalaz._

/** This handler is not thread safe, it's assumed a new one will be created
 * for each client connection.
 *
 * TODO: Pass health monitor to the request handler to report on Netty errors.
 */
private[engines] class HttpServiceUpstreamHandler(service: AsyncHttpService[ByteChunk], executionContext: ExecutionContext) extends SimpleChannelUpstreamHandler with Logging {
  private val pendingResponses = new HashSet[Future[HttpResponse[ByteChunk]]] with SynchronizedSet[Future[HttpResponse[ByteChunk]]]
  private implicit val M: Monad[Future] = new FutureMonad(executionContext)

  override def messageReceived(ctx: ChannelHandlerContext, event: MessageEvent) {
    val request = event.getMessage.asInstanceOf[HttpRequest[ByteChunk]]
    service.service(request) match {
      case Success(responseFuture) =>
        pendingResponses += responseFuture

        for (response <- responseFuture) {
          pendingResponses -= responseFuture
          writeResponse(request, event.getChannel, response)
        }

      case Failure(notServed) => 
        // FIXME: ???
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
    logger.warn("An exception was raised by an I/O thread or a ChannelHandler", e.getCause)

    killPending(e.getCause)
    // e.getChannel.close
  }

  private def writeResponse(request: HttpRequest[ByteChunk], channel: Channel, response: HttpResponse[ByteChunk]) = {
    import HttpNettyConverters._

    val nettyResponse = new DefaultHttpResponse(toNettyVersion(response.version), toNettyStatus(response.status))
    for (header <- response.headers.raw) nettyResponse.setHeader(header._1, header._2)

    if (channel.isConnected) {
      val nettyFuture = response.content match {
        case Some(Left(buffer)) =>
          nettyResponse.setHeader(NettyHttpHeaders.Names.CONTENT_LENGTH, buffer.remaining.toString)
          nettyResponse.setContent(ChannelBuffers.copiedBuffer(buffer))
          channel.write(nettyResponse)

        case Some(Right(stream)) =>
          nettyResponse.setHeader(NettyHttpHeaders.Names.TRANSFER_ENCODING, "chunked")
          channel.write(nettyResponse)
          channel.write(new StreamChunkedInput(stream, channel))

        case None =>
          nettyResponse.setHeader(NettyHttpHeaders.Names.CONTENT_LENGTH, "0")
          channel.write(nettyResponse)
      }

      if (!isKeepAlive(request)) nettyFuture.addListener(ChannelFutureListener.CLOSE)
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
    pendingResponses.foreach(_.asInstanceOf[Promise[HttpResponse[ByteChunk]]].failure(why))
    pendingResponses.clear()
  }
}

private[engines] class StreamChunkedInput(stream: StreamT[Future, ByteBuffer], channel: Channel)(implicit M: Monad[Future]) 
    extends ChunkedInput with Logging with AkkaDefaults {

  private val lock = new ReadWriteLock

  private var data: HttpChunk = null
  @volatile private var isEOF: Boolean = false
  @volatile private var awaitingRead: Boolean = false
  private var remaining: StreamT[Future, ByteBuffer] = null

  advance(stream)

  private def advance(stream: StreamT[Future, ByteBuffer]): Future[Unit] = {
    stream.uncons map { 
      case Some((buffer, tail)) =>
        //
        lock.writeLock { 
          data = new DefaultHttpChunk(ChannelBuffers.wrappedBuffer(buffer))      
          remaining = stream
          awaitingRead = true
          isEOF = false
        }

      case None =>
        lock.writeLock {
          data = new DefaultHttpChunkTrailer
          remaining = null
          awaitingRead = true
          isEOF = true
        }
    } onFailure { 
      case ex =>
        logger.error("An error was encountered retrieving the next chunk of data: " + ex.getMessage, ex)
        lock.writeLock {
          data = null
          remaining = null
          awaitingRead = false
          isEOF = true
        }
    }
  }

  override def close() {
    // FIXME
  }

  override def isEndOfInput() = isEOF

  override def hasNextChunk() = awaitingRead && !isEOF

  override def nextChunk() = {
    lock.writeLock {
      if (!awaitingRead) throw new IllegalStateException("No data available; nextChunk called when not awaiting read.")
      val result = data
      awaitingRead = false
      if (!isEOF) advance(remaining)
      result
    }
  }
}
