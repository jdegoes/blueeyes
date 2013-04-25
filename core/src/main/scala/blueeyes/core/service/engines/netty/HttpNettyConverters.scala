package blueeyes.core.service.engines.netty

import blueeyes.core.data.{ Chunk, ByteChunk }
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpVersions._

import org.jboss.netty.buffer.ChannelBuffer
import org.jboss.netty.handler.codec.http.{
  QueryStringDecoder, 
  HttpResponseStatus, 
  DefaultHttpResponse, 
  HttpVersion => NettyHttpVersion, 
  HttpMethod  => NettyHttpMethod, 
  HttpHeaders => NettyHttpHeaders, 
  HttpRequest => NettyHttpRequest,
  HttpResponse => NettyHttpResponse
}

import java.io.ByteArrayOutputStream
import java.net.{SocketAddress, InetSocketAddress}

import akka.dispatch.Future

import scala.collection.JavaConverters._
import scalaz.syntax.apply._
import scalaz.std.option._

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

  private def getParameters(uri: String): Map[Symbol, String] = {
    try {
      val queryStringDecoder  = new QueryStringDecoder(uri)

      queryStringDecoder.getParameters.asScala map {
        case (k, v) => Symbol(k) -> Option(v).flatMap(_.asScala.headOption).getOrElse("")
      } toMap
    } catch {
      case ex: IllegalArgumentException =>
        throw new HttpException(BadRequest, "An encoding error prevented decoding of query string parameters from the uri %s: %s".format(uri, ex.getMessage))
    }
  }

  private def buildHeaders(nettyHeaders: java.util.List[java.util.Map.Entry[java.lang.String,java.lang.String]]) = {
    nettyHeaders.asScala.foldLeft(HttpHeaders.Empty) {
      case (headers, header) =>
        ^(Option(header.getKey), Option(header.getValue)) { (key, value) =>
          headers + (key -> headers.get(key).map(_ + "," + value).getOrElse(value))
        } getOrElse {
          headers
        }
    }
  }
}
