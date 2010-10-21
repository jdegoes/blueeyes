package blueeyes.core.service

import org.jboss.netty.handler.codec.http.{QueryStringDecoder, HttpResponseStatus, DefaultHttpResponse, HttpMethod => NettyHttpMethod, HttpResponse => NettyHttpResponse, HttpVersion => NettyHttpVersion, HttpRequest => NettyHttpRequest}
import scala.collection.JavaConversions._
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.util.CharsetUtil
import blueeyes.core.data.{DataTranscoder}
import HttpHeaders._
import HttpVersions._

object Converters {
  implicit def fromNettyVersion(version: NettyHttpVersion): HttpVersion = version.getText.toUpperCase match {
    case "HTTP/1.0" => `HTTP/1.0`
    case "HTTP/1.1" => `HTTP/1.1`
  }
  implicit def toNettyVersion(version: HttpVersion): NettyHttpVersion   = NettyHttpVersion.valueOf(version.value)
  implicit def toNettyStatus(status : HttpStatus) : HttpResponseStatus = new HttpResponseStatus(status.code.value, status.reason)
  
  implicit def fromNettyMethod(method: NettyHttpMethod): HttpMethod = method match{
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

  implicit def toNettyResponse[T](response: HttpResponse[T], transcoder: DataTranscoder[String, T]): NettyHttpResponse = {
    val nettyResponse = new DefaultHttpResponse(toNettyVersion(response.version), toNettyStatus(response.status))
    val contentType   = (for (`Content-Type`(t) <- response.headers) yield t).headOption.getOrElse(transcoder.mimeType.value)
    val headers       = response.headers + ("Content-Type" -> transcoder.mimeType.value)

    response.content.foreach(content => nettyResponse.setContent(ChannelBuffers.copiedBuffer(transcoder.transcode.unapply(content), CharsetUtil.UTF_8)))
    headers.foreach(header => nettyResponse.setHeader(header._1, header._2))

    nettyResponse
  }

  implicit def fromNettyRequest[T](request: NettyHttpRequest, pathParameters: Map[Symbol, String], transcoder: DataTranscoder[String, T]): HttpRequest[T] = {
    val queryStringDecoder = new QueryStringDecoder(request.getUri())
    val params             = pathParameters ++ queryStringDecoder.getParameters().map(param => (Symbol(param._1), if (!param._2.isEmpty) param._2.head else "")).toMap
    val headers            = Map(request.getHeaders().map(header => (header.getKey(), header.getValue())): _*)
    val nettyContent       = request.getContent()
    val content            = if (nettyContent.readable()) Some(transcoder.transcode(nettyContent.toString(CharsetUtil.UTF_8))) else None

    HttpRequest(fromNettyMethod(request.getMethod), request.getUri, params, headers, content, fromNettyVersion(request.getProtocolVersion()))
  }
}