package blueeyes.core.service.engines

import org.specs.Specification
import org.jboss.netty.handler.codec.http.{HttpResponseStatus, HttpMethod => NettyHttpMethod, HttpVersion => NettyHttpVersion, DefaultHttpRequest}
import org.jboss.netty.buffer.{ChannelBuffers, ChannelBuffer}
import org.jboss.netty.util.CharsetUtil
import java.net.InetSocketAddress;
import scala.collection.JavaConversions._

import blueeyes.core.http.HttpVersions._
import blueeyes.core.http.{HttpMethods, HttpResponse, HttpStatus, HttpStatusCodes}
import blueeyes.core.data.{Chunk, BijectionsChunkReaderString, MemoryChunk}
import blueeyes.core.http.MimeTypes._

class NettyConvertersSpec extends Specification with NettyConverters with BijectionsChunkReaderString{
  "convert netty method to service method" in {
    fromNettyMethod(NettyHttpMethod.GET) mustEqual(HttpMethods.GET)
  }
  "convert netty version to service version" in {
    fromNettyVersion(NettyHttpVersion.HTTP_1_1) mustEqual(`HTTP/1.1`)
  }
  "convert service version to netty version" in {
    toNettyVersion(`HTTP/1.1`) mustEqual(NettyHttpVersion.HTTP_1_1)
  }
  "convert service HttpStatus to netty HttpStatus" in {
    toNettyStatus(HttpStatus(HttpStatusCodes.NotFound, "missing")) mustEqual(new HttpResponseStatus(HttpStatusCodes.NotFound.value, "missing"))
  }
  "convert service HttpResponse to netty HttpResponse" in {
    val response = HttpResponse[Chunk](HttpStatus(HttpStatusCodes.NotFound), Map("retry-after" -> "1"), Some(StringToChunkReader("12")), `HTTP/1.0`)
    val message  = toNettyResponse(response)

    message.getStatus                               mustEqual(new HttpResponseStatus(HttpStatusCodes.NotFound.value, ""))
    message.getProtocolVersion                      mustEqual(NettyHttpVersion.HTTP_1_0)
    Map(message.getHeaders.map(header => (header.getKey(), header.getValue())): _*)  mustEqual(Map("retry-after" -> "1", "Transfer-Encoding" -> "chunked"))

  }

  "convert netty NettyHttpRequest to service NettyHttpRequest" in {
    val nettyRequest  = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "http://foo/bar?param1=value1")
    nettyRequest.setContent(ChannelBuffers.wrappedBuffer("12".getBytes))
    nettyRequest.setHeader("retry-after", "1")

    val address = new InetSocketAddress("127.0.0.0", 8080)
    val request = fromNettyRequest(nettyRequest, address)

    request.method      mustEqual(HttpMethods.GET)
    request.uri         mustEqual("http://foo/bar?param1=value1")
    request.parameters  mustEqual(Map('param1 -> "value1"))
    request.headers     mustEqual(Map("retry-after" -> "1"))
    request.version     mustEqual(`HTTP/1.0`)
    request.remoteHost  mustEqual(Some(address.getAddress()))
  }

  "convert netty NettyHttpRequest to service NettyHttpRequest, modifying ip if X-Forwarded-For header present" in {
    val nettyRequest  = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "http://foo/bar?param1=value1")
    nettyRequest.setContent(ChannelBuffers.wrappedBuffer("12".getBytes))
    nettyRequest.setHeader("retry-after", "1")
    nettyRequest.setHeader("X-Forwarded-For", "111.11.11.1, 121.21.2.2")

    val address = new InetSocketAddress("127.0.0.0", 8080)
    val forwardedAddress = new InetSocketAddress("111.11.11.1", 8080)
    val request = fromNettyRequest(nettyRequest, address)

    request.method      mustEqual(HttpMethods.GET)
    request.uri         mustEqual("http://foo/bar?param1=value1")
    request.parameters  mustEqual(Map('param1 -> "value1"))
    request.headers     mustEqual(Map("retry-after" -> "1", "X-Forwarded-For" -> "111.11.11.1, 121.21.2.2"))
    request.version     mustEqual(`HTTP/1.0`)
    request.remoteHost  mustEqual(Some(forwardedAddress.getAddress()))
  }

  "convert netty NettyHttpRequest with multiple headers values to service HttpRequest" in {
    val nettyRequest  = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "http://foo/bar?param1=value1")
    nettyRequest.addHeader("retry-after", "1")
    nettyRequest.addHeader("retry-after", "2")

    val request = fromNettyRequest(nettyRequest, new InetSocketAddress("127.0.0.0", 8080))

    request.headers     mustEqual(Map("retry-after" -> "1,2"))
  }

}
