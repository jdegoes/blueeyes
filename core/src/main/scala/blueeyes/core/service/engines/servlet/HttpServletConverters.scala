package blueeyes.core.service.engines.servlet

import blueeyes.bkka._
import blueeyes.core.data.ByteChunk
import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpVersions._

import akka.util.Timeout
import akka.dispatch.Future
import akka.dispatch.ExecutionContext

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.ByteBuffer
import java.net.InetAddress
import javax.servlet.http.HttpServletRequest

import scalaz._
import scala.collection.JavaConverters._

trait HttpServletConverters {
  def maxBufferSize = 8192

  def fromServletRequest(request: HttpServletRequest)(implicit ctx: ExecutionContext): HttpRequest[ByteChunk] = {
    val parameters = request.getParameterMap.asScala.map {
      case (name, values) => Symbol(name) -> values.headOption.getOrElse("")
    }
    
    val headers    = buildHeaders(request)
    val content    = fromServletContent(request)

    val xforwarded  = headers.header(`X-Forwarded-For`).flatMap(_.ips.toList.headOption.map(_.ip))
    val remoteIp    = xforwarded.orElse(headers.header(`X-Cluster-Client-Ip`).flatMap(_.ips.toList.headOption.map(_.ip)))
    val remoteHost  = remoteIp.orElse(Option(request.getRemoteHost).map(InetAddress.getByName(_)))
    val httpRequest = HttpRequest(HttpMethods.parseHttpMethods(request.getMethod).head, 
                                  URI(request.getRequestURL.toString), 
                                  parameters.toMap, headers, content, remoteHost, 
                                  fromServletVersion(request.getProtocol))

    httpRequest.withSubpath(request.getRequestURI.substring(request.getContextPath.length + request.getServletPath.length))
  }

  def fromServletContent(request: HttpServletRequest)(implicit ctx: ExecutionContext): Option[ByteChunk] = {
    if (!isTransferEncodingChunked(request)) {
      val byteContents = new ByteArrayOutputStream()
      val buffer       = new Array[Byte](maxBufferSize)
      val in = request.getInputStream

      var length       = in.read(buffer)
      while (length != -1) {
        byteContents.write(buffer, 0, length);
        length       = in.read(buffer)
      }

      val written = byteContents.toByteArray
      if (written.length == 0) None else Some(Left(written))
    } else {
      implicit val M: Monad[Future] = new FutureMonad(ctx)
      Some(Right(
       StreamT.unfoldM[Future, Array[Byte], InputStream](request.getInputStream) { in =>
         Future {
           val bytes = new Array[Byte](maxBufferSize)
           val length = in.read(bytes)
           if (length < 1) None else Some((bytes, in))
         }
       }
      ))
    }
  }

  private def buildHeaders(request: HttpServletRequest) = {
    request.getHeaderNames.asScala.foldLeft(HttpHeaders.Empty) {
      case (headers, key) =>
        val value = request.getHeader(key)
        headers + (key -> headers.get(key).map(_ + "," + value).getOrElse(value))
    }
  }

  def fromServletVersion(version: String): HttpVersion = version.toUpperCase match {
    case "HTTP/1.0" => `HTTP/1.0`
    case "HTTP/1.1" => `HTTP/1.1`
  }

  def isTransferEncodingChunked(request: HttpServletRequest) = {
    val chunked = request.getHeaders("Transfer-Encoding").asScala.toList
    !chunked.find(_.equalsIgnoreCase("chunked")).isEmpty
  }
}
