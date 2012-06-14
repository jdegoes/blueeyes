package blueeyes.core.service.engines.netty

import akka.dispatch.Future
import akka.dispatch.Promise

import blueeyes.concurrent.ReadWriteLock
import blueeyes.bkka.AkkaDefaults
import blueeyes.core.http._
import blueeyes.core.data.{ByteChunk, Chunk}
import blueeyes.core.service._

import com.weiglewilczek.slf4s.Logger
import com.weiglewilczek.slf4s.Logging

import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.channel.{ SimpleChannelUpstreamHandler, MessageEvent, Channel, ChannelFutureListener, ChannelHandler, ChannelHandlerContext, ChannelStateEvent, ExceptionEvent }
import org.jboss.netty.handler.codec.http.HttpHeaders._
import org.jboss.netty.handler.codec.http.{ DefaultHttpChunkTrailer, DefaultHttpChunk, HttpChunk }

import scala.collection.JavaConversions._
import scala.collection.mutable.{HashSet, SynchronizedSet}

/** This handler is not thread safe, it's assumed a new one will be created
 * for each client connection.
 *
 * TODO: Pass health monitor to the request handler to report on Netty errors.
 */
private[engines] class HttpNettyRequestHandler(requestHandler: AsyncCustomHttpService[ByteChunk], log: Logger) extends SimpleChannelUpstreamHandler with HttpNettyConverters {
  private val pendingResponses = new HashSet[Future[HttpResponse[ByteChunk]]] with SynchronizedSet[Future[HttpResponse[ByteChunk]]]

  override def messageReceived(ctx: ChannelHandlerContext, event: MessageEvent) {
    def writeResponse(e: MessageEvent, response: HttpResponse[ByteChunk]) {
      val chunkedContent  = new ChunkedContent(response.content)
      val keepAlive       = isKeepAlive(e.getMessage.asInstanceOf[HttpRequest[ByteChunk]])
      val message         = toNettyResponse(response, chunkedContent.isChunked)
      val content         = if (!chunkedContent.isChunked) {
                              chunkedContent.chunk.foreach(value => message.setContent(ChannelBuffers.copiedBuffer(value.data)))
                              None
                            } else {
                              chunkedContent.chunk.map(content => new NettyChunkedInput(content, e.getChannel))
                            }

      if (e.getChannel.isConnected){
        val messageFuture = e.getChannel.write(message)
        val contentFuture = content.map(value => e.getChannel.write(value))
        val future        = contentFuture.getOrElse(messageFuture)

        if (!keepAlive) future.addListener(ChannelFutureListener.CLOSE)
      }
    }

    for (responseFuture <- requestHandler.service(event.getMessage.asInstanceOf[HttpRequest[ByteChunk]])) {
      pendingResponses += responseFuture

      for (response <- responseFuture) {
        pendingResponses -= responseFuture
        writeResponse(event, response)
      }
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

  override def channelClosed(ctx: ChannelHandlerContext, e: ChannelStateEvent) = {
    super.channelClosed(ctx, e)

    killPending(new RuntimeException("Channel closed."))
  }

  override def channelDisconnected(ctx: ChannelHandlerContext, e: ChannelStateEvent) = {
    super.channelDisconnected(ctx, e)

    killPending(new RuntimeException("Channel disconnected."))
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {
    log.warn("An exception was raised by an I/O thread or a ChannelHandler", e.getCause)

    killPending(e.getCause)
    // e.getChannel.close
  }

  private def killPending(why: Throwable) {
    // Kill all pending responses to this channel:
    pendingResponses.foreach(_.asInstanceOf[Promise[HttpResponse[ByteChunk]]].failure(why))
    pendingResponses.clear()
  }
}

import org.jboss.netty.handler.stream.ChunkedInput
import org.jboss.netty.handler.stream.ChunkedWriteHandler

private[engines] class NettyChunkedInput(chunk: ByteChunk, channel: Channel) extends ChunkedInput with Logging with AkkaDefaults {
  private var done  = false

  private val lock = new ReadWriteLock{}
  private var nextChunkFuture: Future[ByteChunk] = _

  setNextChunkFuture(Future(chunk))

  def close() {
    nextChunkFuture.asInstanceOf[Promise[ByteChunk]].failure(new RuntimeException("Connection closed."))
  }

  def isEndOfInput = !hasNextChunk()

  def nextChunk = nextChunkFuture.value match {
    case Some(Right(chunk)) =>
      val data = chunk.data
      if (!data.isEmpty) new DefaultHttpChunk(ChannelBuffers.wrappedBuffer(data)) else new DefaultHttpChunkTrailer()

    case Some(Left(ex)) =>
      logger.error("An error was encountered retrieving the next chunk of data: " + ex.getMessage, ex)
      null

    case None =>
      for (_ <- nextChunkFuture) {
        try {
          channel.getPipeline.get(classOf[ChunkedWriteHandler]).resumeTransfer()
        } catch {
          case ex => logger.error("An error was encountered resuming data transfer: " + ex.getMessage, ex)
        }
      }

      null
  }

  def hasNextChunk = {
    nextChunkFuture.value match {
      case Some(Right(chunk)) =>
        chunk.next match {
          case Some(future)     =>
            setNextChunkFuture(future)
            true

          case None if (!done)  => {
            setNextChunkFuture(Future[ByteChunk](Chunk(Array[Byte]())))
            done = true
            true
          }

          case _ => false
        }

      case Some(Left(ex)) =>
        logger.error("An error occurred retrieving the next chunk of data: " + ex.getMessage, ex)
        false

      case _ => true
    }
  }

  private def setNextChunkFuture(future: Future[ByteChunk]){
    lock.writeLock{
      nextChunkFuture = future
    }
  }
}

private[engines] class ChunkedContent(content: Option[ByteChunk]){
  val (chunk, isChunked) = content map { value =>
    val nextChunk = value.next
    nextChunk match {
      case Some(next) => (Some(Chunk(value.data, nextChunk)), true)
      case None       => (content, false)
    }
  } getOrElse ((None, false))
}
