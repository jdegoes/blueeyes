package blueeyes.core.service

import scala.collection.JavaConversions._
import org.jboss.netty.channel.{ChannelFutureListener, ChannelHandlerContext, MessageEvent, SimpleChannelUpstreamHandler}
import org.jboss.netty.handler.codec.http.HttpHeaders.Names
import org.jboss.netty.handler.codec.http.HttpHeaders._
import Converters._
import org.jboss.netty.handler.codec.http.{CookieEncoder, CookieDecoder, HttpRequest => NettyHttpRequest, HttpResponse => NettyHttpResponse, DefaultHttpResponse, HttpVersion => NettyHttpVersion}

import blueeyes.core.http.{HttpMethod, HttpStatusCodes}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.data.{DataTranscoder}
import blueeyes.util.{Future}

class NettyRequestHandler[S](hierarchy: RestHierarchy[S]) extends SimpleChannelUpstreamHandler{
  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
    val request        = e.getMessage().asInstanceOf[NettyHttpRequest]
    val method         = fromNettyMethod(request.getMethod())
    val requestUri     = if (request.getUri().startsWith("/")) request.getUri().substring(1) else request.getUri()

    val handler       = hierarchy.hierarchy.find(v => v._1.isDefinedAt(requestUri) && v._2 == method)
    val builder       = handler.map(v => createResponse(requestUri, v) _).getOrElse(createNotFoundResponse _)

    builder(e).deliverTo(writeResponse(e) _)
  }

  private def createNotFoundResponse(event: MessageEvent) = {
    new Future[NettyHttpResponse]().deliver(new DefaultHttpResponse(NettyHttpVersion.HTTP_1_1, toNettyStatus(HttpStatus(HttpStatusCodes.NotFound))))
  }
  private def createResponse[T, S](requestUri: String, handler: (RestPathPattern, HttpMethod, HttpRequest[T] => Future[HttpResponse[T]], HttpDataTranscoder[T, S]))(event: MessageEvent) = {
    val request     = event.getMessage().asInstanceOf[NettyHttpRequest]
    val parameters  = handler._1(requestUri)

    handler._3(fromNettyRequest(request, parameters, handler._4)).map(response => toNettyResponse(response, handler._4))
  }

  private def writeResponse(e: MessageEvent)(response: NettyHttpResponse){
    val request   = e.getMessage().asInstanceOf[NettyHttpRequest]
    val keepAlive = isKeepAlive(request)
    
    if (keepAlive) response.setHeader(Names.CONTENT_LENGTH, response.getContent().readableBytes())

    val future = e.getChannel().write(response)

    if (!keepAlive) future.addListener(ChannelFutureListener.CLOSE)
  }

  private def encodeCookie(request: NettyHttpRequest, response: NettyHttpResponse) = {
    val cookieString = request.getHeader(Names.COOKIE)
    if (cookieString != null) {
      val cookieDecoder = new CookieDecoder()
      val cookieEncoder = new CookieEncoder(true)
      val cookies       = cookieDecoder.decode(cookieString)
      cookies.foreach(cookieEncoder.addCookie(_))
      response.addHeader(Names.SET_COOKIE, cookieEncoder.encode())
    }
  }
}