package blueeyes.core.service.engines

import org.specs.Specification
import org.jboss.netty.handler.codec.http.{DefaultHttpRequest, HttpChunk, HttpMethod => NettyHttpMethod, HttpVersion => NettyHttpVersion}
import org.jboss.netty.buffer.{HeapChannelBufferFactory, ChannelBuffers}
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import org.specs.mock.MocksCreation
import java.net.{SocketAddress, InetSocketAddress}
import blueeyes.core.http.HttpRequest
import org.jboss.netty.channel._
import blueeyes.core.data.{MemoryChunk, Chunk}
import java.io.ByteArrayOutputStream
import blueeyes.concurrent.{FutureDeliveryStrategySequential, Future}

class NettyChunkedRequestHandlerSpec extends Specification with MocksCreation with NettyConverters with FutureDeliveryStrategySequential{

  private val channel       = mock[Channel]
  private val channelConfig = mock[ChannelConfig]
  private val context       = mock[ChannelHandlerContext]
  private val event         = mock[MessageEvent]
  private val remoteAddress = new InetSocketAddress("127.0.0.0", 8080)
  private val handler       = new NettyChunkedRequestHandler(2)
  private val nettyRequest  = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "/bar/1/adCode.html")

  private val chunkEvent    = mock[MessageEvent]
  private val httpChunk     = mock[HttpChunk]
  private val chunkData     = Array[Byte]('1', '2')

  "NettyChunkedRequestHandler" should {

    when(channel.getConfig()).thenReturn(channelConfig, channelConfig)
    when(channelConfig.getBufferFactory()).thenReturn(HeapChannelBufferFactory.getInstance())
    when(event.getRemoteAddress()).thenReturn(remoteAddress)
    when(event.getChannel()).thenReturn(channel, channel)
    when(context.getChannel()).thenReturn(channel, channel)
    when(event.getMessage()).thenReturn(nettyRequest, nettyRequest)

    when(chunkEvent.getMessage()).thenReturn(httpChunk, httpChunk)
    when(chunkEvent.getChannel()).thenReturn(channel, channel)
    when(chunkEvent.getRemoteAddress()).thenReturn(remoteAddress)
    when(httpChunk.getContent()).thenReturn(ChannelBuffers.wrappedBuffer(chunkData))

    "sends requests as it is when it is not chunked" in {
      nettyRequest.setChunked(false)
      handler.messageReceived(context, event)

      Mockito.verify(context, times(1)).sendUpstream(new UpstreamMessageEventImpl(channel, fromNettyRequest(nettyRequest, remoteAddress), remoteAddress))
    }
    "sends request and chunk when request is chunked and there is only one chunk" in {
      nettyRequest.setChunked(true)
      when(httpChunk.isLast()).thenReturn(true)

      handler.messageReceived(context, event)
      handler.messageReceived(context, chunkEvent)

      val request: HttpRequest[Chunk] = fromNettyRequest(nettyRequest, remoteAddress).copy(content = Some(new MemoryChunk(chunkData)))
      Mockito.verify(context, times(1)).sendUpstream(new UpstreamMessageEventImpl(channel, request, remoteAddress))
    }
    "sends request and chunk when request is chunked and there is only more ther one chunk" in {
      nettyRequest.setChunked(true)

      when(httpChunk.isLast()).thenReturn(false)
      handler.messageReceived(context, event)
      handler.messageReceived(context, chunkEvent)

      when(httpChunk.getContent()).thenReturn(ChannelBuffers.wrappedBuffer(chunkData))
      when(httpChunk.isLast()).thenReturn(true)

      handler.messageReceived(context, chunkEvent)

      val nextChunk = Future.lift[Chunk](new MemoryChunk(chunkData))
      val request: HttpRequest[Chunk] = fromNettyRequest(nettyRequest, remoteAddress).copy(content = Some(new MemoryChunk(chunkData, () => Some(nextChunk))))
      Mockito.verify(context, times(1)).sendUpstream(new UpstreamMessageEventImpl(channel, request, remoteAddress))
    }
  }

  class UpstreamMessageEventImpl(channel: Channel, message: HttpRequest[Chunk], remoteAddress: SocketAddress) extends UpstreamMessageEvent(channel, message, remoteAddress){
    override def equals(p1: Any) = {
      val anotherEvent    = p1.asInstanceOf[UpstreamMessageEvent]
      val anotherMessage  = anotherEvent.getMessage.asInstanceOf[HttpRequest[Chunk]]

      anotherEvent.getChannel == channel && anotherMessage.copy(content = None) == message.copy(content = None) &&
      message.content.map(readContent(_)) == anotherMessage.content.map(readContent(_)) && anotherEvent.getRemoteAddress == remoteAddress
    }

    private def readContent(chunk: Chunk): String = new String(readContent(chunk, new ByteArrayOutputStream()).toByteArray)
    private def readContent(chunk: Chunk, buffer: ByteArrayOutputStream): ByteArrayOutputStream = {
      buffer.write(chunk.data)

      val next = chunk.next
      next match{
        case None =>  buffer
        case Some(x) => readContent(x.value.get, buffer)
      }
    }
  }
}