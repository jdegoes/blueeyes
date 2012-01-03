package blueeyes.core.service.engines


import blueeyes.concurrent.Future._
import blueeyes.core.http._
import blueeyes.core.data.{ByteChunk, MemoryChunk}
import blueeyes.core.service._

import net.lag.logging.Logger

import org.jboss.netty.buffer.{ChannelBuffers}
import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.http.HttpHeaders._

import scala.collection.JavaConversions._
import scala.collection.mutable.{HashSet, SynchronizedSet}
import org.jboss.netty.handler.codec.http.{DefaultHttpChunkTrailer, DefaultHttpChunk, HttpChunk}
import org.jboss.netty.handler.codec.http.websocketx.{TextWebSocketFrame, WebSocketServerHandshaker, WebSocketServerHandshakerFactory}
import blueeyes.concurrent.{ReadWriteLock, Future}

/** This handler is not thread safe, it's assumed a new one will be created
 * for each client connection.
 *
 * TODO: Pass health monitor to the request handler to report on Netty errors.
 */
private[engines] class HttpNettyRequestHandler(requestHandler: AsyncCustomHttpService[ByteChunk], log: Logger) extends SimpleChannelUpstreamHandler with HttpNettyConverters with WebSocketUtils{

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
    event.getMessage match{
      case request: HttpRequest[ByteChunk] =>
        requestHandler.service(request).foreach{ responseFuture =>
          pendingResponses += responseFuture

          responseFuture.deliverTo { response =>
            pendingResponses -= responseFuture

            if (isWebSocketRequest(request.headers) && response.status.code == HttpStatusCodes.OK) startHandshake(request, response, ctx)
            else writeResponse(event, response)
          }
        }
      case _ => writeResponse(event, HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.BadRequest)))
    }
  }

  private def startHandshake(request: HttpRequest[ByteChunk], response: HttpResponse[ByteChunk], ctx: ChannelHandlerContext){
    var wsFactory = new WebSocketServerHandshakerFactory(getWebSocketLocation(request), null, false)
    val handshaker = Option(wsFactory.newHandshaker(ctx, toNettyRequest(request)))
    handshaker match {
      case Some(h) =>
        h.executeOpeningHandshake(ctx, toNettyRequest(request))
        ctx.getChannel.getPipeline.remove("chunkedWriter")
        ctx.getChannel.getPipeline.replace("handler", "wshandler", new HttpNettyWebSocketRequestHandler(response, h))
      case _ => wsFactory.sendUnsupportedWebSocketVersionResponse(ctx)
    }
  }

  private def getWebSocketLocation(request: HttpRequest[ByteChunk]): String = {
    "ws://" + request.headers.get(HttpHeaders.Host.name).getOrElse("")
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

  class ChunkedContent(content: Option[ByteChunk]){
    val (chunk, isChunked) = content map { value =>
      val nextChunk = value.next
      nextChunk match {
        case Some(next) => (Some(new MemoryChunk(value.data, () => {nextChunk})), true)
        case None       => (content, false)
      }
    } getOrElse ((None, false))
  }
}
