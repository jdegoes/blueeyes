package blueeyes.core.service.server

import scala.collection.JavaConversions._
import blueeyes.util.Future
import org.jboss.netty.channel.{ChannelFutureListener, ChannelHandlerContext, MessageEvent, SimpleChannelUpstreamHandler}
import org.jboss.netty.handler.codec.http.HttpHeaders.Names
import org.jboss.netty.handler.codec.http.HttpHeaders._
import blueeyes.core.data.{Bijection}

import blueeyes.core.service._
import Converters._
import org.jboss.netty.handler.codec.http.{CookieEncoder, CookieDecoder, HttpRequest => NettyHttpRequest, HttpResponse => NettyHttpResponse}

class NettyRequestHandler[T](builder: RestHierarchyBuilder[T])(implicit bijection: Bijection[String, T]) extends SimpleChannelUpstreamHandler{
  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
    val request    = e.getMessage().asInstanceOf[NettyHttpRequest]
    val method     = fromNetty(request.getMethod())
    val requestUri = request.getUri().substring(1)

    val handlerF   = builder.hierarchy.find(handler => handler._1.isDefinedAt(requestUri) && method == handler._2).map(handler => handleRequest(handler) _).getOrElse(handleNotFoundRequest _)

    handlerF(e)
  }

  private def handleRequest(handler: (RestPathPattern, HttpMethod, (Map[Symbol, String], HttpRequest[T]) => Future[HttpResponse[T]]))(e: MessageEvent){
    val request       = e.getMessage().asInstanceOf[NettyHttpRequest]
    val parameters    = handler._1(request.getUri())

    val requestFuture = handler._3(parameters, request)
    requestFuture.deliverTo(writeResponse(e) _)
  }

  private def handleNotFoundRequest(e: MessageEvent){
    writeResponse(e)(HttpResponse(HttpStatus(HttpStatusCodes.NotFound)))
  }

  private def writeResponse(e: MessageEvent)(response: HttpResponse[T]){
    val request       = e.getMessage().asInstanceOf[NettyHttpRequest]
    val keepAlive     = isKeepAlive(request)
    
    val nettyResponse = toNetty(response)
    
    if (keepAlive) nettyResponse.setHeader(Names.CONTENT_LENGTH, nettyResponse.getContent().readableBytes())

    val future = e.getChannel().write(nettyResponse)

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
