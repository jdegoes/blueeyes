package blueeyes.core.service.engines

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

trait HttpNettyConverters{
  implicit def fromNettyVersion(version: NettyHttpVersion): HttpVersion = version.getText.toUpperCase match {
    case "HTTP/1.0" => `HTTP/1.0`
    case "HTTP/1.1" => `HTTP/1.1`
  }
  
  implicit def toNettyVersion(version: HttpVersion): NettyHttpVersion   = NettyHttpVersion.valueOf(version.value)
  
  implicit def toNettyStatus(status : HttpStatus) : HttpResponseStatus = status.code match {
    case HttpStatusCodes.OK => HttpResponseStatus.OK
    case _ => new HttpResponseStatus(status.code.value, status.reason)
  }
  
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

  implicit def toNettyResponse(response: HttpResponse[ByteChunk], chunked: Boolean): NettyHttpResponse = {
    val nettyResponse = new DefaultHttpResponse(response.version, response.status)

    response.headers.raw.foreach(header => nettyResponse.setHeader(header._1, header._2))
    val (header, value) = if (chunked) (NettyHttpHeaders.Names.TRANSFER_ENCODING, "chunked") else (NettyHttpHeaders.Names.CONTENT_LENGTH, response.content.map(_.data.length.toString).getOrElse("0"))
    nettyResponse.setHeader(header, value)

    nettyResponse
  }

  implicit def fromNettyRequest(request: NettyHttpRequest, remoteAddres: SocketAddress): HttpRequest[ByteChunk] = {
    val parameters          = getParameters(request.getUri)
    val headers             = buildHeaders(request.getHeaders)
    val content             = fromNettyContent(request.getContent, None)

    val xforwarded  = headers.header(`X-Forwarded-For`).flatMap(_.ips.toList.headOption.map(_.ip))
    val remoteIp    = xforwarded.orElse(headers.header(`X-Cluster-Client-Ip`).flatMap(_.ips.toList.headOption.map(_.ip)))
    val remoteHost  = remoteIp.orElse(Some(remoteAddres).collect { case x: InetSocketAddress => x.getAddress })

    HttpRequest(request.getMethod, URI(request.getUri), parameters, headers, content, remoteHost, fromNettyVersion(request.getProtocolVersion))
  }

  def fromNettyContent(nettyContent: ChannelBuffer, f: Option[Future[ByteChunk]]): Option[ByteChunk] = if (nettyContent.readable()) Some(Chunk(extractContent(nettyContent), f)) else None

  private def extractContent(content: ChannelBuffer) = {
    val stream = new ByteArrayOutputStream()
    try {
      content.readBytes(stream, content.readableBytes)
      stream.toByteArray
    }
    finally stream.close()
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
