package blueeyes.core.service.engines

import org.jboss.netty.handler.codec.http.{QueryStringDecoder, HttpResponseStatus, DefaultHttpResponse, HttpMethod => NettyHttpMethod, HttpResponse => NettyHttpResponse, HttpVersion => NettyHttpVersion, HttpRequest => NettyHttpRequest}
import scala.collection.JavaConversions._
import blueeyes.core.http.HttpIps

import blueeyes.core.http._
import scala.collection.JavaConversions._

import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpVersions._
import org.jboss.netty.buffer.{ChannelBuffer}
import java.net.{SocketAddress, InetSocketAddress}
import blueeyes.core.data.Bijection

trait NettyConvertersions {
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

  implicit def toNettyResponse[T, S](response: HttpResponse[T])(implicit contentBijection: Bijection[ChannelBuffer, T]): NettyHttpResponse = {
    val nettyResponse = new DefaultHttpResponse(response.version, response.status)

    response.content.foreach(content => nettyResponse.setContent(contentBijection.unapply(content)))
    response.headers.foreach(header => nettyResponse.setHeader(header._1, header._2))

    nettyResponse
  }

  implicit def fromNettyRequest[T, S](request: NettyHttpRequest, remoteAddres: SocketAddress)(implicit contentBijection: Bijection[ChannelBuffer, T]): HttpRequest[T] = {
    val queryStringDecoder  = new QueryStringDecoder(request.getUri())
    val parameters          = queryStringDecoder.getParameters().map(param => (Symbol(param._1), if (!param._2.isEmpty) param._2.head else "")).toMap
    val headers             = Map(request.getHeaders().map(header => (header.getKey(), header.getValue())): _*)
    val nettyContent        = request.getContent()
    val content             = if (nettyContent.readable()) Some(contentBijection(nettyContent)) else None
    val remoteHost          = if (headers.contains("x-forwarded-for")) {
                                HttpIps.parseSingleIp(headers("x-forwarded-for")).map(_.ip)
                              } else {
                                remoteAddres match {
                                  case x: InetSocketAddress => Some(x.getAddress())
                                  case _ => None
                                }
                              }

    HttpRequest(request.getMethod, request.getUri, parameters, headers, content, remoteHost, fromNettyVersion(request.getProtocolVersion()))
  }
}
