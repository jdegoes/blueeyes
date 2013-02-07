package blueeyes.core.service
package engines.netty

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.Await

import blueeyes.bkka._
import blueeyes.core.data._
import blueeyes.core.http.HttpRequest

import org.jboss.netty.buffer.{HeapChannelBufferFactory, ChannelBuffers}
import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.http.{DefaultHttpRequest, HttpChunk, HttpMethod => NettyHttpMethod, HttpVersion => NettyHttpVersion}

import java.net.{SocketAddress, InetSocketAddress}
import java.nio.ByteBuffer

import scalaz.StreamT
import scalaz.syntax.monad._

import blueeyes.akka_testing.FutureMatchers

import org.specs2.mock._
import org.specs2.mutable.Specification
import org.specs2.specification.BeforeExample

class HttpNettyChunkedRequestHandlerSpec extends Specification with Mockito with BeforeExample with TestAkkaDefaults with FutureMatchers {
  import HttpNettyConverters._

  private val channel       = mock[Channel]
  private val channelConfig = mock[ChannelConfig]
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
      val context       = mock[ChannelHandlerContext]
      context.getChannel() returns channel
      nettyRequest.setChunked(false)

      handler.messageReceived(context, event)

      val request: HttpRequest[ByteChunk] = fromNettyRequest(nettyRequest, remoteAddress)(None)

      there was one(context).sendUpstream(new UpstreamMessageEventImpl(channel, request, remoteAddress))
    }

    "sends request and chunk when request is chunked and there is only one chunk" in {
      val context       = mock[ChannelHandlerContext]
      context.getChannel() returns channel
      nettyRequest.setChunked(true)
      httpChunk.isLast() returns true

      handler.messageReceived(context, event)
      handler.messageReceived(context, chunkEvent)

      val chunk: ByteChunk = Right(ByteBuffer.wrap(chunkData) :: StreamT.empty[Future, ByteBuffer])
      val request: HttpRequest[ByteChunk] = fromNettyRequest(nettyRequest, remoteAddress)(Some(chunk))

      there was one(context).sendUpstream(new UpstreamMessageEventImpl(channel, request, remoteAddress))
    }

    "sends request and chunk when request is chunked and there is only more ther one chunk" in {
      val context       = mock[ChannelHandlerContext]
      context.getChannel() returns channel
      nettyRequest.setChunked(true)
      httpChunk.isLast() returns false

      handler.messageReceived(context, event)
      handler.messageReceived(context, chunkEvent)

      httpChunk.getContent() returns ChannelBuffers.wrappedBuffer(chunkData)
      httpChunk.isLast() returns true

      handler.messageReceived(context, chunkEvent)

      val chunk: ByteChunk = Right(ByteBuffer.wrap(chunkData) :: Future(ByteBuffer.wrap(chunkData)).liftM[StreamT])
      val request: HttpRequest[ByteChunk] = fromNettyRequest(nettyRequest, remoteAddress)(Some(chunk))

      there was one(context).sendUpstream(new UpstreamMessageEventImpl(channel, request, remoteAddress))
    }
  }

  protected def before = {
    channel.getConfig() returns channelConfig
    channelConfig.getBufferFactory() returns HeapChannelBufferFactory.getInstance()
    event.getRemoteAddress() returns remoteAddress
    event.getChannel() returns channel
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

      val b2 = message.content.map(c => Await.result(ByteChunk.forceByteArray(c), 10 seconds).toList) ==
               anotherMessage.content.map(c => Await.result(ByteChunk.forceByteArray(c), 10 seconds).toList)

      val b3 = anotherEvent.getRemoteAddress == remoteAddress

      b && b1 && b2 && b3
    }
  }
}
