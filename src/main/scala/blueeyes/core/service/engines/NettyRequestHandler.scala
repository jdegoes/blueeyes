package blueeyes.core.service.engines

import scala.collection.JavaConversions._
import scala.collection.mutable.{HashSet, SynchronizedSet}

import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.http.HttpHeaders.Names
import org.jboss.netty.handler.codec.http.HttpHeaders.Names._
import org.jboss.netty.handler.codec.http.HttpHeaders._
import org.jboss.netty.handler.codec.http.{HttpRequest => NettyHttpRequest}

import blueeyes.core.data.{Chunk, MemoryChunk}
import blueeyes.core.service._
import blueeyes.concurrent.{Future, FutureDeliveryStrategySequential}
import blueeyes.concurrent.Future._
import blueeyes.core.http._
import net.lag.logging.Logger

/** This handler is not thread safe, it's assumed a new one will be created 
 * for each client connection.
 *
 * TODO: Pass health monitor to the request handler to report on Netty errors.
 */
private[engines] class NettyRequestHandler(requestHandler: HttpRequestHandler[Chunk], log: Logger) extends SimpleChannelUpstreamHandler with NettyConverters{
  private val pendingResponses = new HashSet[Future[HttpResponse[Chunk]]] with SynchronizedSet[Future[HttpResponse[Chunk]]]

  override def messageReceived(ctx: ChannelHandlerContext, event: MessageEvent) {
    def writeResponse(e: MessageEvent, response: HttpResponse[Chunk]) {
      val request    = e.getMessage().asInstanceOf[NettyHttpRequest]
      val message    = toNettyResponse(response)
      val content    = response.content.map(content => new NettyChunkedInput(content, e.getChannel))
      val keepAlive  = isKeepAlive(request)

      if (e.getChannel().isConnected){
        val messageFuture = e.getChannel().write(message)
        val contentFuture = content.map(value => e.getChannel().write(value))
        val future        = contentFuture.getOrElse(messageFuture)

        if (!keepAlive) future.addListener(ChannelFutureListener.CLOSE)
      }
    }
    val request        = fromNettyRequest(event.getMessage.asInstanceOf[NettyHttpRequest], event.getRemoteAddress)
    val responseFuture = requestHandler(request)

    pendingResponses += responseFuture

    responseFuture.deliverTo { response => 
      pendingResponses -= responseFuture
      
      writeResponse(event, response)
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
  
  private def killPending(why: Option[Throwable]) = {
    // Kill all pending responses to this channel:
    pendingResponses.foreach(_.cancel(why))
    pendingResponses.clear()
  }
}

import NettyChunkedInput._
import org.jboss.netty.handler.stream.ChunkedInput
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.handler.stream.ChunkedWriteHandler
class NettyChunkedInput(chunk: Chunk, channel: Channel) extends ChunkedInput with FutureDeliveryStrategySequential{

  private val log   = Logger.get
  private var done  = false
  private var nextChunkFuture: Future[Chunk] = _

  setNextChunkFuture(Future.lift(chunk))

  def close = {nextChunkFuture.cancel}

  def isEndOfInput = !hasNextChunk()

  def nextChunk = {
    nextChunkFuture.value.map{chunk =>
      val data = chunk.data
      ChannelBuffers.wrappedBuffer(Integer.toHexString(data.length).getBytes ++ CRLF ++ data ++ CRLF)
    }.getOrElse{
      nextChunkFuture.deliverTo{nextChunk =>
        channel.getPipeline().get(classOf[ChunkedWriteHandler]).resumeTransfer
        channel.write(new NettyChunkedInput(nextChunk, channel))
      }
      null
    }
  }

  def hasNextChunk = {
    nextChunkFuture.value.map{chunk =>
      chunk.next match{
        case Some(future)     => setNextChunkFuture(future)
          true
        case None if (!done)  => {
          setNextChunkFuture(Future.lift[Chunk](new MemoryChunk(Array[Byte]())))
          done = true
          true
        }
        case _ => false
      }
    }.getOrElse(true)
  }

  private def setNextChunkFuture(future: Future[Chunk]){
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
