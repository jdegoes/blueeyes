package blueeyes.core.service

import org.jboss.netty.handler.codec.http.{QueryStringDecoder, HttpResponseStatus, DefaultHttpResponse, HttpMethod => NettyHttpMethod, HttpResponse => NettyHttpResponse, HttpVersion => NettyHttpVersion, HttpRequest => NettyHttpRequest}
import scala.collection.JavaConversions._
import org.jboss.netty.util.CharsetUtil
import blueeyes.core.http._
import scala.collection.JavaConversions._

import HttpHeaders._
import HttpVersions._
import org.jboss.netty.buffer.{ChannelBuffer, ChannelBuffers}

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

  implicit def toNettyResponse[T, S](response: HttpResponse[T], transcoder: HttpDataTranscoder[T, S]): NettyHttpResponse = {
    val nettyResponse = new DefaultHttpResponse(toNettyVersion(response.version), toNettyStatus(response.status))
    val contentType   = (for (`Content-Type`(t) <- response.headers) yield t).headOption.getOrElse(transcoder.mimeType.value)
    val headers       = response.headers + ("Content-Type" -> transcoder.mimeType.value)

    response.content.foreach(content => nettyResponse.setContent(channelBuffer(content, transcoder)))
    headers.foreach(header => nettyResponse.setHeader(header._1, header._2))

    nettyResponse
  }

  private def channelBuffer[T, S](content: T, transcoder: HttpDataTranscoder[T, S]): ChannelBuffer = transcoder.responseType match {
    case HttpResponseStringType => ChannelBuffers.copiedBuffer(transcoder.transcode.apply(content).asInstanceOf[String], CharsetUtil.UTF_8)
    case HttpResponseBytesType  => ChannelBuffers.copiedBuffer(transcoder.transcode.apply(content).asInstanceOf[Array[Byte]])
  }
  private def fromChannelBuffer[T, S](content: ChannelBuffer, transcoder: HttpDataTranscoder[T, S]): T = transcoder.responseType match {
    case HttpResponseStringType => transcoder.transcode.unapply(content.toString(CharsetUtil.UTF_8).asInstanceOf[S])
    case HttpResponseBytesType  => {
      val arrayContent: Array[Byte] = content.array()
      transcoder.transcode.unapply(arrayContent.asInstanceOf[S])
    }
  }

  implicit def fromNettyRequest[T, S](request: NettyHttpRequest, pathParameters: Map[Symbol, String], transcoder: HttpDataTranscoder[T, S]): HttpRequest[T] = {
    val queryStringDecoder = new QueryStringDecoder(request.getUri())
    val params             = pathParameters ++ queryStringDecoder.getParameters().map(param => (Symbol(param._1), if (!param._2.isEmpty) param._2.head else "")).toMap
    val headers            = Map(request.getHeaders().map(header => (header.getKey(), header.getValue())): _*)
    val nettyContent       = request.getContent()
    val content            = if (nettyContent.readable()) Some(fromChannelBuffer(nettyContent, transcoder)) else None

    HttpRequest(fromNettyMethod(request.getMethod), request.getUri, params, headers, content, fromNettyVersion(request.getProtocolVersion()))
  }
}
