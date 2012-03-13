package blueeyes.core.service.engines.servlet

import scala.collection.JavaConversions._
import javax.servlet.http.HttpServletRequest
import blueeyes.core.data.{ByteChunk, Chunk}
import java.net.InetAddress
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpVersions._
import java.io.{ByteArrayOutputStream, InputStream}
import blueeyes.core.http._
import blueeyes.bkka.AkkaDefaults
import akka.util.Timeout
import akka.dispatch.{Await, Future}

trait HttpServletConverters {
  def fromServletRequest(request: HttpServletRequest): HttpRequest[ByteChunk] = {
    val parameters = request.getParameterMap.map{param => (Symbol(param._1), if (!param._2.isEmpty) param._2.head else "")}.toMap
    val headers    = buildHeaders(request)
    val content    = fromServletContent(request)

    val xforwarded  = headers.header(`X-Forwarded-For`).flatMap(_.ips.toList.headOption.map(_.ip))
    val remoteIp    = xforwarded.orElse(headers.header(`X-Cluster-Client-Ip`).flatMap(_.ips.toList.headOption.map(_.ip)))
    val remoteHost  = remoteIp.orElse(Option(request.getRemoteHost).map(InetAddress.getByName(_)))
    val httpRequest = HttpRequest(HttpMethods.parseHttpMethods(request.getMethod).head, URI(request.getRequestURL.toString), parameters, headers, content, remoteHost, fromServletVersion(request.getProtocol))

    httpRequest.withSubpath(request.getRequestURI.substring(request.getContextPath.length + request.getServletPath.length))
  }

  def fromServletContent(request: HttpServletRequest): Option[ByteChunk] = {
    if (!isTransferEncodingChunked(request)){
      val content = extractContent(request.getInputStream)
      if (!content.isEmpty) Some(Chunk(content)) else None
    }
    else{
      InputStreamSource(request.getInputStream)
    }
  }

  private def extractContent(in: InputStream) = {
    val byteContents = new ByteArrayOutputStream()
    val buffer       = new Array[Byte](8192)

    var length       = in.read(buffer)
    while (length != -1) {
      byteContents.write(buffer, 0, length);
      length       = in.read(buffer)
    }

    byteContents.flush()
    byteContents.toByteArray
  }


  private def buildHeaders(request: HttpServletRequest) = {
    request.getHeaderNames.foldLeft(HttpHeaders.Empty) {
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
    val chunked = request.getHeaders("Transfer-Encoding").toList
    !chunked.find(_.equalsIgnoreCase("chunked")).isEmpty
  }
}

private[servlet] class InputStreamSource(inputStream: InputStream) extends AkkaDefaults{
  private val buffer       = new Array[Byte](8192)
  def apply: Option[Future[ByteChunk]] = {
    val length = inputStream.read(buffer)
    if (length > -1){
      val data = new Array[Byte](length)
      buffer.copyToArray(data, 0, length)
      Some(Future(Chunk(data, apply)))
    }
    else
      None
  }
}

private[servlet] object InputStreamSource{
  def apply(inputStream: InputStream, timeout: akka.util.Timeout = Timeout.never): Option[ByteChunk] = new InputStreamSource(inputStream).apply.map(future => Await.result(future, timeout.duration))
}
