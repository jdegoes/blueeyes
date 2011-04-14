package blueeyes.core.service.engines

import scala.collection.JavaConversions._
import scala.collection.mutable.{HashSet, SynchronizedSet}

import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.http.HttpHeaders.Names
import org.jboss.netty.handler.codec.http.HttpHeaders.Names._
import org.jboss.netty.handler.codec.http.HttpHeaders._
import org.jboss.netty.handler.codec.http.{HttpRequest => NettyHttpRequest}

import blueeyes.core.data.ChunkReader
import blueeyes.core.service._
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import blueeyes.core.http._
import net.lag.logging.Logger

/** This handler is not thread safe, it's assumed a new one will be created 
 * for each client connection.
 *
 * TODO: Pass health monitor to the request handler to report on Netty errors.
 */
private[engines] class NettyRequestHandler(requestHandler: HttpRequestHandler[ChunkReader], log: Logger) extends SimpleChannelUpstreamHandler with NettyConverters{
  private val pendingResponses = new HashSet[Future[HttpResponse[ChunkReader]]] with SynchronizedSet[Future[HttpResponse[ChunkReader]]]

  override def messageReceived(ctx: ChannelHandlerContext, event: MessageEvent) {
    def writeResponse(e: MessageEvent, response: HttpResponse[ChunkReader]) {
      val request             = e.getMessage().asInstanceOf[NettyHttpRequest]
      val (message, content)  = toNettyResponse(response)
      val keepAlive           = isKeepAlive(request)

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