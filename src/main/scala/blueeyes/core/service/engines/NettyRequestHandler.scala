package blueeyes.core.service.engines

import scala.collection.JavaConversions._
import scala.collection.mutable.{HashSet, SynchronizedSet}

import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.http.HttpHeaders.Names
import org.jboss.netty.handler.codec.http.HttpHeaders.Names._
import org.jboss.netty.handler.codec.http.HttpHeaders._
import org.jboss.netty.buffer.{ChannelBuffer}
import org.jboss.netty.handler.timeout.TimeoutException
import org.jboss.netty.handler.codec.http.{HttpRequest => NettyHttpRequest}

import blueeyes.util.RichThrowableImplicits._
import blueeyes.core.data.Bijection
import blueeyes.core.service._
import blueeyes.util.Future
import blueeyes.util.Future._
import blueeyes.core.http._
import net.lag.logging.Logger

/** This handler is not thread safe, it's assumed a new one will be created 
 * for each client connection.
 *
 * TODO: Pass health monitor to the request handler to report on Netty errors.
 */
private[engines] class NettyRequestHandler[T] (requestHandler: HttpRequestHandler[T], log: Logger)(implicit contentBijection: Bijection[ChannelBuffer, T]) extends SimpleChannelUpstreamHandler with NettyConverters{
  private val pendingResponses = new HashSet[Future[HttpResponse[T]]] with SynchronizedSet[Future[HttpResponse[T]]]
  
  private lazy val NotFound            = HttpResponse[T](HttpStatus(HttpStatusCodes.NotFound))
  private lazy val InternalServerError = HttpResponse[T](HttpStatus(HttpStatusCodes.InternalServerError))

  override def messageReceived(ctx: ChannelHandlerContext, event: MessageEvent) {
    def convertErrorToResponse(th: Throwable): HttpResponse[T] = th match {
      case e: HttpException => HttpResponse[T](HttpStatus(e.failure, e.reason))
      case _ => {
        val reason = th.fullStackTrace
        
        HttpResponse[T](HttpStatus(HttpStatusCodes.InternalServerError, if (reason.length > 3500) reason.substring(0, 3500) else reason))
      }
    }
    
    def writeResponse(e: MessageEvent, response: HttpResponse[T]) {
      val request       = e.getMessage().asInstanceOf[NettyHttpRequest]
      val nettyResponse = toNettyResponse(response)
      val keepAlive     = isKeepAlive(request)

      if (keepAlive) nettyResponse.setHeader(Names.CONTENT_LENGTH, nettyResponse.getContent().readableBytes())

      val future = e.getChannel().write(nettyResponse)

      if (!keepAlive) future.addListener(ChannelFutureListener.CLOSE)
    }
    
    val request = fromNettyRequest(event.getMessage.asInstanceOf[NettyHttpRequest], event.getRemoteAddress)
    
    val responseFuture = {
      // The raw future may die due to error:
      val rawFuture = try {
        if (requestHandler.isDefinedAt(request)) requestHandler(request)
        else Future[HttpResponse[T]](NotFound)
      }
      catch {
        case why: Throwable => 
          // An error during invocation of the request handler, convert to
          // proper response:
          Future[HttpResponse[T]](convertErrorToResponse(why))
      }
      
      // Convert the raw future into one that cannot die:
      rawFuture.orElse { why =>
        try { event.getChannel.close } catch { case e: Throwable => log.error(e, "Error closing channel") }
        
        why match {
          case Some(throwable) =>
            convertErrorToResponse(throwable)

          case None =>
            // Future was canceled for no cause.
            InternalServerError
        }
      }
    }

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
    log.error(e.getCause, "ExceptionCaught")
    
    killPending(Some(e.getCause))
    
    e.getChannel.close    
  }
  
  private def killPending(why: Option[Throwable]) = {
    // Kill all pending responses to this channel:
    pendingResponses.foreach(_.cancel(why))
    pendingResponses.clear()
  }
}