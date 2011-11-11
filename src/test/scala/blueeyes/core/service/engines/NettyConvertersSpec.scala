package blueeyes.core.service.engines

import org.specs2.mutable.Specification

import org.jboss.netty.handler.codec.http.{HttpResponseStatus, HttpMethod => NettyHttpMethod, HttpVersion => NettyHttpVersion, DefaultHttpRequest}
import org.jboss.netty.buffer.{ChannelBuffers, ChannelBuffer}
import org.jboss.netty.util.CharsetUtil
import java.net.InetSocketAddress
import blueeyes.core.http._
;
import scala.collection.JavaConversions._

import blueeyes.core.http.HttpVersions._
import blueeyes.core.data.{ByteChunk, BijectionsChunkString}
import blueeyes.core.http.MimeTypes._
import org.specs2.execute.PendingUntilFixed

class NettyConvertersSpec extends Specification with PendingUntilFixed with NettyConverters with BijectionsChunkString{
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
    val response = HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.NotFound), Map("Retry-After" -> "1"), Some(StringToChunk("12")), `HTTP/1.0`)
    val message  = toNettyResponse(response, true)

    message.getStatus                               mustEqual(new HttpResponseStatus(HttpStatusCodes.NotFound.value, ""))
    message.getProtocolVersion                      mustEqual(NettyHttpVersion.HTTP_1_0)
    Map(message.getHeaders.map(header => (header.getKey, header.getValue)): _*)  mustEqual(Map("Retry-After" -> "1", "Transfer-Encoding" -> "chunked"))

  }
  "convert service HttpResponse to netty HttpResponse with not chunked content" in {
    val response = HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.NotFound), Map(), None, `HTTP/1.0`)
    val message  = toNettyResponse(response, false)

    message.getStatus                               mustEqual(new HttpResponseStatus(HttpStatusCodes.NotFound.value, ""))
    message.getProtocolVersion                      mustEqual(NettyHttpVersion.HTTP_1_0)
    Map(message.getHeaders.map(header => (header.getKey, header.getValue)): _*)  mustEqual(Map("Content-Length" -> "0"))
  }

  "convert netty NettyHttpRequest to service NettyHttpRequest" in {
    val nettyRequest  = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "http://foo/bar%20foo?param1=foo%20bar")
    nettyRequest.setContent(ChannelBuffers.wrappedBuffer("12".getBytes))
    nettyRequest.setHeader("Retry-After", "1")

    val address = new InetSocketAddress("127.0.0.0", 8080)
    val request = fromNettyRequest(nettyRequest, address)

    request.method       mustEqual(HttpMethods.GET)
    request.parameters   mustEqual(Map('param1 -> "foo bar"))
    request.uri          mustEqual(URI("http://foo/bar%20foo?param1=foo%20bar"))
    request.headers.raw  mustEqual(Map("Retry-After" -> "1"))
    request.version      mustEqual(`HTTP/1.0`)
    request.remoteHost   mustEqual(Some(address.getAddress))
  }

  "convert netty NettyHttpRequest to service NettyHttpRequest, modifying ip if X-Forwarded-For header present" in {
    val nettyRequest  = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "http://foo/bar?param1=value1")
    nettyRequest.setContent(ChannelBuffers.wrappedBuffer("12".getBytes))
    nettyRequest.setHeader("Retry-After", "1")
    nettyRequest.setHeader("X-Forwarded-For", "111.11.11.1, 121.21.2.2")

    val address = new InetSocketAddress("127.0.0.0", 8080)
    val forwardedAddress = new InetSocketAddress("111.11.11.1", 8080)
    val request = fromNettyRequest(nettyRequest, address)

    request.method      mustEqual(HttpMethods.GET)
    request.uri         mustEqual(URI("http://foo/bar?param1=value1"))
    request.parameters  mustEqual(Map('param1 -> "value1"))
    request.headers.raw mustEqual(Map("Retry-After" -> "1", "X-Forwarded-For" -> "111.11.11.1, 121.21.2.2"))
    request.version     mustEqual(`HTTP/1.0`)
    request.remoteHost  mustEqual(Some(forwardedAddress.getAddress))
  }
  "convert netty NettyHttpRequest to service NettyHttpRequest, modifying ip if X-Cluster-Client-Ip header present" in {
    val nettyRequest  = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "http://foo/bar?param1=value1")
    nettyRequest.setHeader("X-Cluster-Client-Ip", "111.11.11.1, 121.21.2.2")

    val address = new InetSocketAddress("127.0.0.0", 8080)
    val forwardedAddress = new InetSocketAddress("111.11.11.1", 8080)
    val request = fromNettyRequest(nettyRequest, address)

    request.method      mustEqual(HttpMethods.GET)
    request.uri         mustEqual(URI("http://foo/bar?param1=value1"))
    request.headers.raw mustEqual(Map("X-Cluster-Client-Ip" -> "111.11.11.1, 121.21.2.2"))
    request.remoteHost  mustEqual(Some(forwardedAddress.getAddress))
  }

  "does not use host name from Host header" in {
    val nettyRequest  = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "http://foo/bar?param1=value1")
    nettyRequest.addHeader("Host", "google.com")

    val request = fromNettyRequest(nettyRequest, new InetSocketAddress("127.0.0.0", 8080))

    request.uri mustEqual(URI("http://foo/bar?param1=value1"))
  }

  "convert netty NettyHttpRequest with multiple headers values to service HttpRequest" in {
    val nettyRequest  = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "http://foo/bar?param1=value1")
    nettyRequest.addHeader("TCodings", "1")
    nettyRequest.addHeader("TCodings", "2")

    val request = fromNettyRequest(nettyRequest, new InetSocketAddress("127.0.0.0", 8080))

    request.headers.raw mustEqual(Map("TCodings" -> "1,2"))
  }
}
