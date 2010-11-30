package blueeyes.core.service.engines

import org.specs.Specification
import org.jboss.netty.handler.codec.http.{HttpMethod, DefaultHttpRequest, HttpVersion}

class FullURIHttpRequestDecoderSpec extends Specification{
  "creates full uri" in{
    val decoder = new FullURIHttpRequestDecoder("http", "google", 8080)
    val message = decoder.createMessage(Array("GET", "/foo", "HTTP/1.1")).asInstanceOf[DefaultHttpRequest]

    message.getMethod           mustEqual(HttpMethod.GET)
    message.getUri              mustEqual("http://google:8080/foo")
    message.getProtocolVersion  mustEqual(HttpVersion.HTTP_1_1)
  }
  "creates full uri when first slash is missing" in{
    val decoder = new FullURIHttpRequestDecoder("http", "google", 8080)
    val message = decoder.createMessage(Array("GET", "foo", "HTTP/1.1")).asInstanceOf[DefaultHttpRequest]

    message.getUri              mustEqual("http://google:8080/foo")
  }
  "creates full uri when original uri is full" in{
    val decoder = new FullURIHttpRequestDecoder("http", "google", 8080)
    val message = decoder.createMessage(Array("GET", "http://google:8080/foo", "HTTP/1.1")).asInstanceOf[DefaultHttpRequest]

    message.getUri              mustEqual("http://google:8080/foo")
  }
}