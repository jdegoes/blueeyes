package blueeyes.core.service.engines.netty

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.Await
import blueeyes.bkka.AkkaDefaults
import blueeyes.concurrent.test.FutureMatchers

import org.specs2.mutable.Specification
import org.jboss.netty.handler.codec.http.{DefaultHttpRequest, HttpChunk, HttpMethod => NettyHttpMethod, HttpVersion => NettyHttpVersion}
import org.jboss.netty.buffer.{HeapChannelBufferFactory, ChannelBuffers}
import java.net.{SocketAddress, InetSocketAddress}
import blueeyes.core.http.HttpRequest
import org.jboss.netty.channel._
import blueeyes.core.data.{Chunk, ByteChunk}
import collection.mutable.ArrayBuilder.ofByte
import org.specs2.mock._
import org.specs2.specification.BeforeExample

class HttpNettyChunkedRequestHandlerSpec extends Specification with Mockito with HttpNettyConverters with BeforeExample with AkkaDefaults with FutureMatchers {

  private val channel       = mock[Channel]
  private val channelConfig = mock[ChannelConfig]
  private val context       = mock[ChannelHandlerContext]
  private val event         = mock[MessageEvent]
  private val remoteAddress = new InetSocketAddress("127.0.0.0", 8080)
  private val handler       = new HttpNettyChunkedRequestHandler(2)
  private val nettyRequest  = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "/bar/1/adCode.html")

  private val chunkEvent    = mock[MessageEvent]
  private val httpChunk     = mock[HttpChunk]
  private val chunkData     = Array[Byte]('1', '2')

  override def is = args(sequential = true) ^ super.is
  "NettyChunkedRequestHandler" should {
    "sends requests as it is when it is not chunked" in {
      nettyRequest.setChunked(false)
      handler.messageReceived(context, event)

      there was one(context).sendUpstream(new UpstreamMessageEventImpl(channel, fromNettyRequest(nettyRequest, remoteAddress), remoteAddress))
    }
    "sends request and chunk when request is chunked and there is only one chunk" in {
      nettyRequest.setChunked(true)
      httpChunk.isLast() returns true

      handler.messageReceived(context, event)
      handler.messageReceived(context, chunkEvent)

      val request: HttpRequest[ByteChunk] = fromNettyRequest(nettyRequest, remoteAddress).copy(content = Some(Chunk(chunkData)))
      there was one(context).sendUpstream(new UpstreamMessageEventImpl(channel, request, remoteAddress))
    }
    "sends request and chunk when request is chunked and there is only more ther one chunk" in {
      nettyRequest.setChunked(true)

      httpChunk.isLast() returns false
      handler.messageReceived(context, event)
      handler.messageReceived(context, chunkEvent)

      httpChunk.getContent() returns ChannelBuffers.wrappedBuffer(chunkData)
      httpChunk.isLast() returns true

      handler.messageReceived(context, chunkEvent)

      val nextChunk = Promise.successful[ByteChunk](Chunk(chunkData))
      val request: HttpRequest[ByteChunk] = fromNettyRequest(nettyRequest, remoteAddress).copy(content = Some(Chunk(chunkData, Some(nextChunk))))

      there was one(context).sendUpstream(new UpstreamMessageEventImpl(channel, request, remoteAddress))
    }
  }

  protected def before = {
    channel.getConfig() returns channelConfig
    channelConfig.getBufferFactory() returns HeapChannelBufferFactory.getInstance()
    event.getRemoteAddress() returns remoteAddress
    event.getChannel() returns channel
    context.getChannel() returns channel
    event.getMessage() returns nettyRequest

    chunkEvent.getMessage() returns httpChunk
    chunkEvent.getChannel() returns channel
    chunkEvent.getRemoteAddress() returns remoteAddress
    httpChunk.getContent() returns ChannelBuffers.wrappedBuffer(chunkData)
  }

  class UpstreamMessageEventImpl(channel: Channel, message: HttpRequest[ByteChunk], remoteAddress: SocketAddress) extends UpstreamMessageEvent(channel, message, remoteAddress){
    override def equals(p1: Any) = {
      val anotherEvent    = p1.asInstanceOf[UpstreamMessageEvent]
      val anotherMessage  = anotherEvent.getMessage.asInstanceOf[HttpRequest[ByteChunk]]

      val b = anotherEvent.getChannel == channel
      val b1 = anotherMessage.copy(content = None) == message.copy(content = None)
      val b2 = message.content.map(readContent(_)) == anotherMessage.content.map(readContent(_))
      val b3 = anotherEvent.getRemoteAddress == remoteAddress
      b && b1 &&
      b2 && b3
    }

    private def readContent(chunk: ByteChunk): String = new String(readContent(chunk, new ofByte()).result)
    private def readContent(chunk: ByteChunk, buffer: ofByte): ofByte = {
      buffer ++= chunk.data

      val next = chunk.next
      next match{
        case None =>  buffer
        case Some(x) => readContent(Await.result(x, 10 seconds), buffer)
      }
    }
  }
}
