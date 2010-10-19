package blueeyes.core.service.server

import scala.collection.JavaConversions._
import org.jboss.netty.channel.{ChannelFutureListener, ChannelHandlerContext, MessageEvent, SimpleChannelUpstreamHandler}
import org.jboss.netty.handler.codec.http.HttpHeaders.Names
import org.jboss.netty.handler.codec.http.HttpHeaders._
import blueeyes.core.service._
import Converters._
import org.jboss.netty.handler.codec.http.{CookieEncoder, CookieDecoder, HttpRequest => NettyHttpRequest, HttpResponse => NettyHttpResponse, DefaultHttpResponse, HttpVersion}
import blueeyes.core.data.{DataTranscoder}
import blueeyes.util.{Future}

class NettyRequestHandler[T](hierarchies: List[(RestHierarchy[T], DataTranscoder[String, T])]) extends SimpleChannelUpstreamHandler{
  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
    val request        = e.getMessage().asInstanceOf[NettyHttpRequest]
    val method         = fromNetty(request.getMethod())
    val requestUri     = if (request.getUri().startsWith("/")) request.getUri().substring(1) else request.getUri()

    val builders       = hierarchies.map(v => new RequestBuilder(e, v._1, v._2))
    val builder        = builders.find(_.isDefinedAt((requestUri, method))).getOrElse(new NotFoundBuilder())

    builder((requestUri, method)).deliverTo(writeResponse(e) _)
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

class NotFoundBuilder extends Function1[(String, HttpMethod), Future[NettyHttpResponse]]{
  def apply(uriAndMethod: (String, HttpMethod)) = {
    val future = new Future[NettyHttpResponse]()
    future.deliver(new DefaultHttpResponse(HttpVersion.HTTP_1_1, toNetty(HttpStatus(HttpStatusCodes.NotFound))))
    future
  }
}

class RequestBuilder[T](e: MessageEvent, hierarchy: RestHierarchy[T], transcoder: DataTranscoder[String, T]) extends PartialFunction[(String, HttpMethod), Future[NettyHttpResponse]]{
  def isDefinedAt(uriAndMethod: (String, HttpMethod)) = findPattern(uriAndMethod._1, uriAndMethod._2).map(v => true).getOrElse(false)
  def apply(uriAndMethod: (String, HttpMethod)) = {
    def handler     = findPattern(uriAndMethod._1, uriAndMethod._2).get
    val request     = e.getMessage().asInstanceOf[NettyHttpRequest]
    val parameters  = handler._1(uriAndMethod._1)

    handler._3(parameters, fromNetty(request, transcoder.transcode)).map(response => toNetty(response, transcoder))
  }
  private def findPattern(uri: String, method: HttpMethod) = hierarchy.hierarchy.find(handler => handler._1.isDefinedAt(uri) && method == handler._2)
}
