package blueeyes.core.service

import org.specs.Specification
import org.jboss.netty.handler.codec.http.{HttpResponseStatus, HttpMethod => NettyHttpMethod, HttpVersion => NettyHttpVersion, DefaultHttpRequest}
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.util.CharsetUtil
import java.net.InetSocketAddress;
import scala.collection.JavaConversions._
import blueeyes.core.data.{DataTranscoder, DataTranscoderImpl, TextToTextBijection}

import Converters._
import blueeyes.core.http.HttpVersions._
import blueeyes.core.http.{HttpMethod, HttpVersion, HttpMethods, HttpVersions, HttpRequest, HttpResponse, HttpStatusCode, HttpStatus, HttpStatusCodes}
import blueeyes.core.http.MimeTypes._

class ConvertersSpec extends Specification {
  private val transcoder = new HttpStringDataTranscoder(TextToTextBijection, text / html)
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
    val response      = HttpResponse[String](HttpStatus(HttpStatusCodes.NotFound), Map("retry-after" -> "1"), Some("12"), `HTTP/1.0`)
    val nettyResponse = toNettyResponse(response, transcoder)

    nettyResponse.getStatus                               mustEqual(new HttpResponseStatus(HttpStatusCodes.NotFound.value, ""))
    nettyResponse.getContent.toString(CharsetUtil.UTF_8)  mustEqual("12")
    nettyResponse.getProtocolVersion                      mustEqual(NettyHttpVersion.HTTP_1_0)
    Map(nettyResponse.getHeaders.map(header => (header.getKey(), header.getValue())): _*)  mustEqual(Map("retry-after" -> "1", "Content-Type" -> "text/html"))
  }
  "convert netty NettyHttpRequest to service HttpRequest" in {
    val nettyRequest  = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "http://foo/bar?param1=value1")
    nettyRequest.setContent(ChannelBuffers.wrappedBuffer("12".getBytes))
    nettyRequest.setHeader("retry-after", "1")

    val address = new InetSocketAddress("127.0.0.0", 8080)
    val request = fromNettyRequest(nettyRequest, Map('pathParam1 -> "value"), address, transcoder)

    request.method      mustEqual(HttpMethods.GET)
    request.uri         mustEqual("http://foo/bar?param1=value1")
    request.parameters  mustEqual(Map('param1 -> "value1", 'pathParam1 -> "value"))
    request.headers     mustEqual(Map("retry-after" -> "1"))
    request.content     mustEqual(Some("12"))
    request.version     mustEqual(`HTTP/1.0`)
    request.remoteHost  mustEqual(Some(address.getAddress()))
  }
  "convert netty NettyHttpRequest with multiple jeaders values to service HttpRequest" in {
    val nettyRequest  = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "http://foo/bar?param1=value1")
    nettyRequest.setContent(ChannelBuffers.wrappedBuffer("12".getBytes))
    nettyRequest.addHeader("retry-after", "1")
    nettyRequest.addHeader("retry-after", "2")

    val address = new InetSocketAddress("127.0.0.0", 8080)
    val request = fromNettyRequest(nettyRequest, Map('pathParam1 -> "value"), address, transcoder)

    request.method      mustEqual(HttpMethods.GET)
    request.uri         mustEqual("http://foo/bar?param1=value1")
    request.parameters  mustEqual(Map('param1 -> "value1", 'pathParam1 -> "value"))
    request.headers     mustEqual(Map("retry-after" -> "1,2"))
    request.content     mustEqual(Some("12"))
    request.version     mustEqual(`HTTP/1.0`)
    request.remoteHost  mustEqual(Some(address.getAddress()))
  }
}
