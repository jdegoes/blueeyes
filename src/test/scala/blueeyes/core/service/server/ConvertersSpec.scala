package blueeyes.core.service.server

import org.specs.Specification
import org.jboss.netty.handler.codec.http.{HttpResponseStatus, HttpMethod => NettyHttpMethod, HttpVersion => NettyHttpVersion, DefaultHttpRequest}
import org.jboss.netty.buffer.ChannelBuffers
import blueeyes.core.service._
import org.jboss.netty.util.CharsetUtil
import blueeyes.core.data.Bijection
import java.lang.String;
import scala.collection.JavaConversions._
import Converters._

class ConvertersSpec extends Specification {
  private val textToInt = new Bijection[String, Int]{
    def unapply(s: Int) = s.toString
    def apply(t: String) = t.toInt
  }

  "convert netty method to service method" in {
    fromNetty(NettyHttpMethod.GET) mustEqual(HttpMethods.GET)
  }
  "convert netty version to service version" in {
    fromNetty(NettyHttpVersion.HTTP_1_1) mustEqual(HttpVersions.Http_1_1)
  }
  "convert service version to netty version" in {
    toNetty(HttpVersions.Http_1_1) mustEqual(NettyHttpVersion.HTTP_1_1)
  }
  "convert service HttpStatus to netty HttpStatus" in {
    toNetty(HttpStatus(HttpStatusCodes.NotFound, "missing")) mustEqual(new HttpResponseStatus(HttpStatusCodes.NotFound.value, "missing"))
  }
  "convert service HttpResponse to netty HttpResponse" in {
    val response      = HttpResponse[Int](HttpStatus(HttpStatusCodes.NotFound), Map("retry-after" -> "1"), Some(12), HttpVersions.Http_1_0)
    val nettyResponse = toNetty(response)(textToInt)

    nettyResponse.getStatus                               mustEqual(new HttpResponseStatus(HttpStatusCodes.NotFound.value, ""))
    nettyResponse.getContent.toString(CharsetUtil.UTF_8)  mustEqual("12")
    nettyResponse.getProtocolVersion                      mustEqual(NettyHttpVersion.HTTP_1_0)
    Map(nettyResponse.getHeaders.map(header => (header.getKey(), header.getValue())): _*)  mustEqual(Map("retry-after" -> "1"))
  }
  "convert netty NettyHttpRequest to service NettyHttpRequest" in {
    val nettyRequest  = new DefaultHttpRequest(NettyHttpVersion.HTTP_1_0, NettyHttpMethod.GET, "http://foo/bar?param1=value1")
    nettyRequest.setContent(ChannelBuffers.wrappedBuffer("12".getBytes))
    nettyRequest.setHeader("retry-after", "1")

    val request = fromNetty(nettyRequest)(textToInt)
    
    request.method      mustEqual(HttpMethods.GET)
    request.uri         mustEqual("http://foo/bar?param1=value1")
    request.parameters  mustEqual(Map("param1" -> "value1"))
    request.headers     mustEqual(Map("retry-after" -> "1"))
    request.content     mustEqual(Some(12))
    request.version     mustEqual(HttpVersions.Http_1_0)
  }
}