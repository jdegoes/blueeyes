package blueeyes.core.service.server

import blueeyes.core.service._
import org.jboss.netty.handler.codec.http.{QueryStringDecoder, HttpResponseStatus, DefaultHttpResponse, HttpMethod => NettyHttpMethod, HttpVersion => NettyHttpVersion, HttpRequest => NettyHttpRequest}
import scala.collection.JavaConversions._
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.util.CharsetUtil;

object Converters{
  implicit def fromNetty(method: NettyHttpMethod): HttpMethod = method match{
    case NettyHttpMethod.GET      => HttpMethods.GET
    case NettyHttpMethod.PUT      => HttpMethods.PUT
    case NettyHttpMethod.POST     => HttpMethods.POST
    case NettyHttpMethod.DELETE   => HttpMethods.DELETE
    case NettyHttpMethod.OPTIONS  => HttpMethods.OPTIONS
    case NettyHttpMethod.HEAD     => HttpMethods.HEAD
    case NettyHttpMethod.CONNECT  => HttpMethods.CONNECT
    case NettyHttpMethod.TRACE    => HttpMethods.TRACE
    case NettyHttpMethod.PATCH    => HttpMethods.PATCH
    case _ => HttpMethods.CUSTOM(method.getName)
  }

  implicit def fromNetty(version: NettyHttpVersion): HttpVersion = HttpVersion(version.getText())
  implicit def toNetty(version: HttpVersion): NettyHttpVersion   = NettyHttpVersion.valueOf(version.value)
  implicit def toNetty(status : HttpStatus) : HttpResponseStatus = new HttpResponseStatus(status.code.value, status.reason)

  implicit def toNetty[T](response: HttpResponse[T])(implicit converter: (T) => String): DefaultHttpResponse = {
    val nettyResponse = new DefaultHttpResponse(toNetty(response.version), toNetty(response.status))
    
    response.content.foreach(content => nettyResponse.setContent(ChannelBuffers.copiedBuffer(converter(content), CharsetUtil.UTF_8)))

    response.headers.foreach(header => nettyResponse.setHeader(header._1, header._2))

    nettyResponse
  }

  implicit def fromNetty[T](request: NettyHttpRequest)(implicit converter: (String) => T): HttpRequest[T] = {
    val queryStringDecoder = new QueryStringDecoder(request.getUri())
    val params             = queryStringDecoder.getParameters().map(param => (param._1, if (!param._2.isEmpty) param._2.head else "")).toMap
    val headers            = Map(request.getHeaders().map(header => (header.getKey(), header.getValue())): _*)
    val nettyContent       = request.getContent()
    val content            = if (nettyContent.readable()) Some(converter(nettyContent.toString(CharsetUtil.UTF_8))) else None

    HttpRequest(fromNetty(request.getMethod), request.getUri, params, headers, content, fromNetty(request.getProtocolVersion()))
  }
}