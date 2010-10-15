package blueeyes.core.service.server

import org.jboss.netty.bootstrap.ServerBootstrap
import java.net.InetSocketAddress
import java.util.concurrent.Executors
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import org.jboss.netty.channel._
import org.jboss.netty.util.CharsetUtil
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.handler.codec.http._

object HTTPServer {
  def main(args: Array[String]) {
    val bootstrap = new ServerBootstrap(new NioServerSocketChannelFactory(Executors.newCachedThreadPool(), Executors.newCachedThreadPool()))

    bootstrap.setPipelineFactory(new HttpServerPipelineFactory())

    bootstrap.bind(new InetSocketAddress(8080))
  }
}

class HttpServerPipelineFactory extends ChannelPipelineFactory {
  def getPipeline(): ChannelPipeline = {

    val pipeline = Channels.pipeline()

    //    val sslEngine = SecureChatSslContextFactory.getServerContext().createSSLEngine()
    //    engine.setUseClientMode(false)
    //    pipeline.addLast("ssl", new SslHandler(engine))

    pipeline.addLast("decoder", new HttpRequestDecoder())
    pipeline.addLast("encoder", new HttpResponseEncoder())
    // Remove the following line if you don't want automatic content compression.
    //    pipeline.addLast("deflater", new HttpContentCompressor())
    pipeline.addLast("handler", new HttpRequestHandler())

    pipeline
  }
}

import scala.collection.JavaConversions._
import org.jboss.netty.handler.codec.http.HttpHeaders._;
import org.jboss.netty.handler.codec.http.HttpHeaders.Names._;
import org.jboss.netty.handler.codec.http.HttpResponseStatus._;
import org.jboss.netty.handler.codec.http.HttpVersion._;

class HttpRequestHandler extends SimpleChannelUpstreamHandler {
  private val buf = new StringBuilder();

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
    val request = e.getMessage().asInstanceOf[HttpRequest]
    buf.setLength(0);
    buf.append("WELCOME TO THE WILD WILD WEB SERVER\r\n")
    buf.append("===================================\r\n")

    buf.append("VERSION: " + request.getProtocolVersion() + "\r\n")
    buf.append("REQUEST_URI: " + request.getUri() + "\r\n\r\n")

    request.getHeaders().foreach(h => buf.append("HEADER: " + h.getKey() + " = " + h.getValue() + "\r\n"))
    buf.append("\r\n")

    val queryStringDecoder = new QueryStringDecoder(request.getUri())
    val params = queryStringDecoder.getParameters()

    params.foreach(p => p._2.foreach(p1 => buf.append("PARAM: " + p._1 + " = " + p1 + "\r\n")))
    buf.append("\r\n")

    val content = request.getContent()
    if (content.readable()) buf.append("CONTENT: " + content.toString(CharsetUtil.UTF_8) + "\r\n")

    writeResponse(request, e)
  }

  private def writeResponse (request: HttpRequest, e: MessageEvent) {
    // Decide whether to close the connection or not.
    val keepAlive = isKeepAlive(request)

    // Build the response object.
    val response = new DefaultHttpResponse(HTTP_1_1, OK)
    response.setContent(ChannelBuffers.copiedBuffer(buf.toString(), CharsetUtil.UTF_8))
    response.setHeader(CONTENT_TYPE, "text/plain; charset=UTF-8")

    if (keepAlive) {
      // Add 'Content-Length' header only for a keep-alive connection.
      response.setHeader(CONTENT_LENGTH, response.getContent().readableBytes())
    }

    // Encode the cookie.
    val cookieString = request.getHeader(COOKIE)
    if (cookieString != null) {
      val cookieDecoder = new CookieDecoder()
      val cookieEncoder = new CookieEncoder(true)
      val cookies       = cookieDecoder.decode(cookieString)
      cookies.foreach(cookieEncoder.addCookie(_))
      response.addHeader(SET_COOKIE, cookieEncoder.encode())
    }

    // Write the response.
    val future = e.getChannel().write(response)

    // Close the non-keep-alive connection after the write operation is done.
    if (!keepAlive) {
      future.addListener(ChannelFutureListener.CLOSE)
    }
  }
}