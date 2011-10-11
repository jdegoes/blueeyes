package blueeyes.core.service.engines


import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import blueeyes.core.http._
import blueeyes.core.data.{GZIPByteChunk, ByteChunk, MemoryChunk}
import blueeyes.core.service._

import net.lag.logging.Logger

import org.jboss.netty.buffer.{ChannelBuffer, ChannelBuffers}
import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.http.HttpHeaders._

import scala.collection.JavaConversions._
import scala.collection.mutable.{HashSet, SynchronizedSet}
import scalaz.{Failure, Success}

/** This handler is not thread safe, it's assumed a new one will be created
 * for each client connection.
 *
 * TODO: Pass health monitor to the request handler to report on Netty errors.
 */
private[engines] class NettyRequestHandler(requestHandler: AsyncCustomHttpService[ByteChunk], log: Logger) extends SimpleChannelUpstreamHandler with NettyConverters{
  private val pendingResponses = new HashSet[Future[HttpResponse[ByteChunk]]] with SynchronizedSet[Future[HttpResponse[ByteChunk]]]

  override def messageReceived(ctx: ChannelHandlerContext, event: MessageEvent) {
    def writeResponse(e: MessageEvent, response: HttpResponse[ByteChunk]) {
      val chunkedContent  = new ChunkedContent(response.content)
      val keepAlive       = isKeepAlive(e.getMessage.asInstanceOf[HttpRequest[ByteChunk]])
      val message         = toNettyResponse(response, chunkedContent.isChunked)
      val content         = if (!chunkedContent.isChunked) {
        chunkedContent.chunk.foreach(value => message.setContent(ChannelBuffers.copiedBuffer(value.data)))
        None
      }
      else chunkedContent.chunk.map(content => new NettyChunkedInput(content, e.getChannel))

      if (e.getChannel.isConnected){
        val messageFuture = e.getChannel.write(message)
        val contentFuture = content.map(value => e.getChannel.write(value))
        val future        = contentFuture.getOrElse(messageFuture)

        if (!keepAlive) future.addListener(ChannelFutureListener.CLOSE)
      }
    }
    requestHandler.service(event.getMessage.asInstanceOf[HttpRequest[ByteChunk]]).foreach{ responseFuture =>
      pendingResponses += responseFuture

      responseFuture.deliverTo { response =>
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

    killPending(None)
  }

  override def channelDisconnected(ctx: ChannelHandlerContext, e: ChannelStateEvent) = {
    super.channelDisconnected(ctx, e)
    
    killPending(None)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {
    log.warning(e.getCause, "An exception was raised by an I/O thread or a ChannelHandler")
    
    killPending(Some(e.getCause))
    // e.getChannel.close    
  }
  
  private def killPending(why: Option[Throwable]) {
    // Kill all pending responses to this channel:
    pendingResponses.foreach(_.cancel(why))
    pendingResponses.clear()
  }
}

import NettyChunkedInput._
import org.jboss.netty.handler.stream.ChunkedInput
import org.jboss.netty.handler.stream.ChunkedWriteHandler
class NettyChunkedInput(chunk: ByteChunk, channel: Channel) extends ChunkedInput{

  private val log   = Logger.get
  private var done  = false
  private var nextChunkFuture: Future[ByteChunk] = _

  setNextChunkFuture(Future.sync(chunk))

  def close() {nextChunkFuture.cancel()}

  def isEndOfInput = !hasNextChunk()

  def nextChunk = {
    nextChunkFuture.value.map{chunk =>
      val data = chunk.data
      ChannelBuffers.wrappedBuffer(Integer.toHexString(data.length).getBytes ++ CRLF ++ data ++ CRLF)
    }.orElse{
      nextChunkFuture.deliverTo{nextChunk =>
        channel.getPipeline.get(classOf[ChunkedWriteHandler]).resumeTransfer()
        channel.write(new NettyChunkedInput(nextChunk, channel))
      }
      None
    }.orNull
  }

  def hasNextChunk = {
    nextChunkFuture.value.map{chunk =>
      chunk.next match{
        case Some(future)     => setNextChunkFuture(future)
          true
        case None if (!done)  => {
          setNextChunkFuture(Future.sync[ByteChunk](new MemoryChunk(Array[Byte]())))
          done = true
          true
        }
        case _ => false
      }
    }.getOrElse(true)
  }

  private def setNextChunkFuture(future: Future[ByteChunk]){
    future.trap{errors: List[Throwable] =>
      errors.foreach(error => log.warning(error, "An exception was raised by NettyChunkedInput"))
      errors match{
        case x :: xs => throw x
        case _ =>
      }
    }
    nextChunkFuture = future
  }
}
object NettyChunkedInput{
 val CRLF = List[Byte]('\r', '\n')
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
