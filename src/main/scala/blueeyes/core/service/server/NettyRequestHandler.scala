package blueeyes.core.service.server

import blueeyes.util.Future
import org.jboss.netty.handler.codec.http.{HttpRequest => NettyHttpRequest}
import org.jboss.netty.channel.{ChannelFutureListener, ChannelHandlerContext, MessageEvent, SimpleChannelUpstreamHandler}
import org.jboss.netty.handler.codec.http.HttpHeaders._;
import blueeyes.core.service._
import Converters._

class NettyRequestHandler[T](builder: RestHierarchyBuilder[T])(implicit stringToType: (String) => T, typeToString: (T) => String) extends SimpleChannelUpstreamHandler{
  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
    val request    = e.getMessage().asInstanceOf[NettyHttpRequest]
    val method     = fromNetty(request.getMethod())
    val requestUri = request.getUri()

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
    val keepAlive = isKeepAlive(e.getMessage().asInstanceOf[NettyHttpRequest])
    val future    = e.getChannel().write(toNetty(response))

    if (!keepAlive) {
      future.addListener(ChannelFutureListener.CLOSE)
    }
  }
}
