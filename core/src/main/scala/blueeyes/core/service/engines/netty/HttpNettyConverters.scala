package blueeyes.core.service.engines.netty

import org.jboss.netty.handler.codec.http.{HttpHeaders => NettyHttpHeaders, QueryStringDecoder, HttpResponseStatus, DefaultHttpResponse, HttpMethod => NettyHttpMethod, HttpResponse => NettyHttpResponse, HttpVersion => NettyHttpVersion, HttpRequest => NettyHttpRequest}

import blueeyes.core.http._
import scala.collection.JavaConversions._
import java.io.ByteArrayOutputStream
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpVersions._
import org.jboss.netty.buffer.ChannelBuffer
import java.net.{SocketAddress, InetSocketAddress}
import blueeyes.core.data.{ Chunk, ByteChunk }
import akka.dispatch.Future

object HttpNettyConverters {
  def fromNettyVersion(version: NettyHttpVersion): HttpVersion = version.getText.toUpperCase match {
    case "HTTP/1.0" => `HTTP/1.0`
    case "HTTP/1.1" => `HTTP/1.1`
  }
  
  def toNettyVersion(version: HttpVersion): NettyHttpVersion   = NettyHttpVersion.valueOf(version.value)
  
  def toNettyStatus(status : HttpStatus) : HttpResponseStatus = status.code match {
    case HttpStatusCodes.OK => HttpResponseStatus.OK
    case _ => new HttpResponseStatus(status.code.value, status.reason)
  }
  
  def fromNettyMethod(method: NettyHttpMethod): HttpMethod = method match{
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

  def fromNettyRequest(request: NettyHttpRequest, remoteAddress: SocketAddress): Option[ByteChunk] => HttpRequest[ByteChunk] = {
    (content: Option[ByteChunk]) => {
      val parameters          = getParameters(request.getUri)
      val headers             = buildHeaders(request.getHeaders)

      val xforwarded  = headers.header(`X-Forwarded-For`).flatMap(_.ips.toList.headOption.map(_.ip))
      val remoteIp    = xforwarded.orElse(headers.header(`X-Cluster-Client-Ip`).flatMap(_.ips.toList.headOption.map(_.ip)))
      val remoteHost  = remoteIp.orElse(Some(remoteAddress).collect { case x: InetSocketAddress => x.getAddress })
      val version     = fromNettyVersion(request.getProtocolVersion)

      HttpRequest(fromNettyMethod(request.getMethod), URI(request.getUri), parameters, headers, content, remoteHost, version)
    }
  }

  private def getParameters(uri: String) = {
    val queryStringDecoder  = new QueryStringDecoder(uri)
    queryStringDecoder.getParameters.map(param => (Symbol(param._1), if (!param._2.isEmpty) param._2.head else "")).toMap
  }

  private def buildHeaders(nettyHeaders: java.util.List[java.util.Map.Entry[java.lang.String,java.lang.String]]) = {
    nettyHeaders.foldLeft(HttpHeaders.Empty) {
      case (headers, header) =>
        val key   = header.getKey
        val value = header.getValue

        headers + (key -> headers.get(key).map(_ + "," + value).getOrElse(value))
    }
  }
}
