package blueeyes.core.service.engines

import scala.collection.JavaConversions._
import org.jboss.netty.channel.Channels._
import org.jboss.netty.handler.codec.http.HttpHeaders._

import org.jboss.netty.buffer.{ChannelBuffer, ChannelBuffers}
import org.jboss.netty.util.CharsetUtil
import org.jboss.netty.handler.codec.http.{HttpHeaders, HttpChunk, HttpRequest => NettyHttpRequest}
import org.jboss.netty.channel._
import NettyChunkedRequestHandler._
import blueeyes.concurrent.Future
import blueeyes.core.data.{MemoryChunk, ByteChunk}
import blueeyes.core.http.HttpRequest
import net.lag.logging.Logger

class NettyChunkedRequestHandler(chunkSize: Int) extends SimpleChannelUpstreamHandler with NettyConverters{

  private val log = Logger.get
  private var delivery: Option[(Either[HttpRequest[ByteChunk], Future[ByteChunk]], ChannelBuffer)] = None

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
    def buffer  = ChannelBuffers.dynamicBuffer(e.getChannel().getConfig().getBufferFactory())
    val current = delivery

    e.getMessage() match {
      case m: NettyHttpRequest => {
        if (is100ContinueExpected(m)) write(ctx, succeededFuture(ctx.getChannel()), CONTINUE.duplicate())

        if (m.isChunked()) {
          List[String]() ++ m.getHeaders(HttpHeaders.Names.TRANSFER_ENCODING) match {
            case HttpHeaders.Values.CHUNKED :: Nil => m.removeHeader(HttpHeaders.Names.TRANSFER_ENCODING)
            case _ =>
          }
          m.setChunked(false)
          delivery = Some(Left(fromNettyRequest(m, e.getRemoteAddress)), buffer)
        } else {
          delivery = None
          Channels.fireMessageReceived(ctx, fromNettyRequest(m, e.getRemoteAddress), e.getRemoteAddress())
        }
      }
      case chunk: HttpChunk =>  {
        current.foreach{ value =>
          val (nextDelivery, content) = value
          content.writeBytes(chunk.getContent())
          if (chunk.isLast || content.capacity >= chunkSize) {
            val nextChunkFuture = if (!chunk.isLast){
              val future = new Future[ByteChunk]()
              delivery   = Some(Right(future), buffer)
              Some(future)
            }
            else{
              delivery = None
              None
            }
            val chunkToSend = fromNettyContent(content, () => nextChunkFuture)
            nextDelivery match {
              case Left(x)  => Channels.fireMessageReceived(ctx, x.copy(content = chunkToSend), e.getRemoteAddress())
              case Right(x) => x.deliver(chunkToSend.getOrElse(new MemoryChunk(Array[Byte]())))
            }
          }
        }
      }
      case _ => write(ctx, succeededFuture(ctx.getChannel()), BAD_REQUEST.duplicate())
    }
  }

  override def channelClosed(ctx: ChannelHandlerContext, e: ChannelStateEvent) = {
    super.channelClosed(ctx, e)
    killPending
  }

  override def channelDisconnected(ctx: ChannelHandlerContext, e: ChannelStateEvent) = {
    super.channelDisconnected(ctx, e)
    killPending
  }

  private def killPending = {
    delivery.foreach{ value =>
      value._1 match {
        case Right(x) => x.cancel
        case Left(x)  =>
      }
    }
    delivery = None
  }
}

object NettyChunkedRequestHandler{
  val CONTINUE: ChannelBuffer    = ChannelBuffers.copiedBuffer("HTTP/1.1 100 Continue\r\n\r\n", CharsetUtil.US_ASCII)
  val BAD_REQUEST: ChannelBuffer = ChannelBuffers.copiedBuffer("HTTP/1.1 400 BadRequest\r\n\r\n", CharsetUtil.US_ASCII)
}
