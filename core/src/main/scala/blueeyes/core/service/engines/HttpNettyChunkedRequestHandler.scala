package blueeyes.core.service.engines

import akka.dispatch.Future
import akka.dispatch.Promise
import blueeyes.bkka.AkkaDefaults

import org.jboss.netty.buffer.{ChannelBuffer, ChannelBuffers}
import org.jboss.netty.channel._
import org.jboss.netty.channel.Channels._
import org.jboss.netty.handler.codec.http.{HttpHeaders => NettyHeaders, HttpChunk => NettyChunk, HttpRequest => NettyRequest}
import org.jboss.netty.util.CharsetUtil

import blueeyes.core.data.{ Chunk, ByteChunk }
import blueeyes.core.http.HttpRequest

import scala.collection.JavaConverters._

private[engines] class HttpNettyChunkedRequestHandler(chunkSize: Int) extends SimpleChannelUpstreamHandler with HttpNettyConverters with AkkaDefaults {
  import HttpNettyChunkedRequestHandler._
  import NettyHeaders._

  private var delivery: Option[(Either[HttpRequest[ByteChunk], Promise[ByteChunk]], ChannelBuffer)] = None

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
    def buffer  = ChannelBuffers.dynamicBuffer(e.getChannel.getConfig.getBufferFactory)
    val current = delivery

    e.getMessage match {
      case nettyRequest: NettyRequest => 
        if (is100ContinueExpected(nettyRequest)) write(ctx, succeededFuture(ctx.getChannel), CONTINUE.duplicate())

        if (nettyRequest.isChunked) {
          nettyRequest.setChunked(false)
          if (nettyRequest.getHeaders(NettyHeaders.Names.TRANSFER_ENCODING).asScala.exists(_ == NettyHeaders.Values.CHUNKED)) {
            nettyRequest.removeHeader(NettyHeaders.Names.TRANSFER_ENCODING)
          }

          delivery = Some(Left(fromNettyRequest(nettyRequest, e.getRemoteAddress)), buffer)
        } else {
          delivery = None
          Channels.fireMessageReceived(ctx, fromNettyRequest(nettyRequest, e.getRemoteAddress), e.getRemoteAddress)
        }

      case chunk: NettyChunk =>  
        for ((nextDelivery, content) <- current) {
          content.writeBytes(chunk.getContent)
          if (chunk.isLast || content.capacity >= chunkSize) {
            val nextChunkFuture = if (!chunk.isLast) {
              val future = Promise[ByteChunk]()
              delivery   = Some(Right(future), buffer)
              Some(future)
            } else {
              delivery = None
              None
            }

            val chunkToSend = fromNettyContent(content, nextChunkFuture)
            nextDelivery match {
              case Left(x)  => Channels.fireMessageReceived(ctx, x.copy(content = chunkToSend), e.getRemoteAddress)
              case Right(x) => x.success(chunkToSend.getOrElse(Chunk(Array[Byte]())))
            }
          }
        }

      case _ => 
        write(ctx, succeededFuture(ctx.getChannel), BAD_REQUEST.duplicate())
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
    delivery.foreach{ value =>
      value._1 match {
        case Right(x) => x.failure(new RuntimeException(message))
        case Left(x)  =>
      }
    }
    delivery = None
  }
}

object HttpNettyChunkedRequestHandler {
  val CONTINUE: ChannelBuffer    = ChannelBuffers.copiedBuffer("HTTP/1.1 100 Continue\r\n\r\n", CharsetUtil.US_ASCII)
  val BAD_REQUEST: ChannelBuffer = ChannelBuffers.copiedBuffer("HTTP/1.1 400 BadRequest\r\n\r\n", CharsetUtil.US_ASCII)
}
