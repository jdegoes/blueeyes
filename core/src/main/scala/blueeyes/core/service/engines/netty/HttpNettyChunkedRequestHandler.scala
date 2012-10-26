package blueeyes.core.service
package engines.netty

import blueeyes.bkka._
import blueeyes.core.data._
import blueeyes.core.http.HttpRequest

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext

import org.jboss.netty.buffer.{ChannelBuffer, ChannelBuffers}
import org.jboss.netty.channel.ChannelHandlerContext
import org.jboss.netty.channel.ChannelStateEvent
import org.jboss.netty.channel.Channels
import org.jboss.netty.channel.MessageEvent
import org.jboss.netty.channel.SimpleChannelUpstreamHandler
import org.jboss.netty.handler.codec.http.{HttpHeaders => NettyHeaders, HttpChunk => NettyChunk, HttpRequest => NettyRequest}
import org.jboss.netty.util.CharsetUtil

import java.nio.ByteBuffer
import java.util.concurrent.CountDownLatch

import scalaz._
import scala.collection.JavaConverters._

private[engines] class HttpNettyChunkedRequestHandler(chunkSize: Int)(implicit executionContext: ExecutionContext) extends SimpleChannelUpstreamHandler {
  import HttpNettyConverters._
  import HttpNettyChunkedRequestHandler._
  import NettyHeaders._

  implicit val M: Monad[Future] = new FutureMonad(executionContext)
  private var chain: Chain = _

  override def messageReceived(ctx: ChannelHandlerContext, event: MessageEvent) {
    event.getMessage match {
      case nettyRequest: NettyRequest => 
        if (is100ContinueExpected(nettyRequest)) Channels.write(ctx, Channels.succeededFuture(ctx.getChannel), CONTINUE.duplicate())

        val httpRequestBuilder = fromNettyRequest(nettyRequest, event.getRemoteAddress)
        val nettyContent = nettyRequest.getContent

        val content: Option[ByteChunk] = if (nettyRequest.isChunked) {
          val head = Chain.incomplete
          if (nettyContent.readable()) {
            chain = Chain.incomplete
            head.promise.success(Some((nettyContent.toByteBuffer, chain)))
          } else {
            chain = head
          }

          Some(Right(StreamT.unfoldM[Future, ByteBuffer, Chain](head) { _.promise }))
        } else {
          if (nettyContent.readable()) {
            Some(Left(nettyContent.toByteBuffer)) 
          } else {
            None
          }
        }

        Channels.fireMessageReceived(ctx, httpRequestBuilder(content), event.getRemoteAddress)

      case chunk: NettyChunk =>  
        val current = chain
        chain = if (chunk.isLast) Chain.complete else Chain.incomplete
        current.promise.success(Some((chunk.getContent.toByteBuffer, chain)))

      case _ =>
        Channels.write(ctx, Channels.succeededFuture(ctx.getChannel), BAD_REQUEST.duplicate())
    }
  }

  override def channelClosed(ctx: ChannelHandlerContext, e: ChannelStateEvent) {
    super.channelClosed(ctx, e)
    killPending("Channel closed.")
  }

  override def channelDisconnected(ctx: ChannelHandlerContext, e: ChannelStateEvent) {
    super.channelDisconnected(ctx, e)
    killPending("Channel disconnected.")
  }

  private def killPending(message: String) {
    if (chain != null) chain.promise.failure(new RuntimeException(message))
  }
}

object HttpNettyChunkedRequestHandler {
  val CONTINUE: ChannelBuffer    = ChannelBuffers.copiedBuffer("HTTP/1.1 100 Continue\r\n\r\n", CharsetUtil.US_ASCII)
  val BAD_REQUEST: ChannelBuffer = ChannelBuffers.copiedBuffer("HTTP/1.1 400 BadRequest\r\n\r\n", CharsetUtil.US_ASCII)
}
