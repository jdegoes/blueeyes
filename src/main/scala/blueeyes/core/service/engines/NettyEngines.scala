package blueeyes.core.service.engines

import blueeyes.core.service._
import org.jboss.netty.handler.codec.http.HttpHeaders.Names
import org.jboss.netty.handler.codec.http.HttpHeaders._
import org.jboss.netty.util.CharsetUtil
import blueeyes.core.data.Bijection
import java.lang.String
import org.jboss.netty.buffer.{ChannelBuffers, ChannelBuffer}
import blueeyes.util.Future
import blueeyes.core.http.{HttpStatusCodes, HttpStatus, HttpResponse, HttpRequest}
import org.jboss.netty.bootstrap.ServerBootstrap
import java.net.InetSocketAddress
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import java.util.concurrent.{Executors, Executor}
import org.jboss.netty.handler.codec.http.{HttpResponseEncoder, HttpRequestDecoder, HttpRequest => NettyHttpRequest}
import org.jboss.netty.channel._
import org.jboss.netty.util.internal.ExecutorUtil
import net.lag.logging.Logger

trait NettyEngine[T] extends HttpServerEngine[T] with HttpServer[T]{ self =>

  private val startStopLock = new java.util.concurrent.locks.ReentrantReadWriteLock

  private var server: Option[ServerBootstrap]  = None
  private var serverExecutor: Option[Executor] = None

  override def start: Future[Unit] = {
    super.start.map(_ => {

      startStopLock.writeLock.lock()

      try {
        val executor = Executors.newCachedThreadPool()
        val bootstrap = new ServerBootstrap(new NioServerSocketChannelFactory(executor, executor))

        bootstrap.setPipelineFactory(new HttpServerPipelineFactory(new NettyRequestHandler[T](self, log)))

        bootstrap.bind(new InetSocketAddress(self.port))

        server = Some(bootstrap)
        serverExecutor = Some(executor)
      }
      finally{
        startStopLock.writeLock.unlock()
      }

      log.info("Netty engine is started using port: " + self.port)
      ()
    })
  }

  override def stop: Future[Unit] = {
    super.stop.map(_ => {

      startStopLock.writeLock.lock()
      
      try {
        serverExecutor.foreach(ExecutorUtil.terminate(_))
        server.foreach(_.releaseExternalResources)
      }
      finally{
        startStopLock.writeLock.unlock()
      }

      log.info("Netty engine is stopped.")
      ()
    })
  }

  implicit def contentBijection: Bijection[ChannelBuffer, T]
}

class NettyRequestHandler[T] (requestHandler: HttpRequestHandler[T], log: Logger)(implicit contentBijection: Bijection[ChannelBuffer, T]) extends SimpleChannelUpstreamHandler with NettyConverters{
  override def messageReceived(ctx: ChannelHandlerContext, event: MessageEvent) {
    val nettyRequest   = event.getMessage().asInstanceOf[NettyHttpRequest]
    val request        = fromNettyRequest(nettyRequest, event.getRemoteAddress)

    handleRequest(request).deliverTo(writeResponse(event) _)
  }

  private def handleRequest(request: HttpRequest[T]) = {
    try {
      if (requestHandler.isDefinedAt(request)) requestHandler(request)
      else new Future[HttpResponse[T]]().deliver(HttpResponse(HttpStatus(HttpStatusCodes.NotFound)))
    }
    catch {
      case e: Throwable => log.error(e, "Error while request handling. Request=" + request); new Future[HttpResponse[T]]().deliver(HttpResponse(HttpStatus(HttpStatusCodes.InternalServerError, e.getMessage.replace("\n", " "))))
    }
  }

  private def writeResponse(e: MessageEvent)(response: HttpResponse[T]){
    val request       = e.getMessage().asInstanceOf[NettyHttpRequest]
    val nettyResponse = toNettyResponse(response)
    val keepAlive     = isKeepAlive(request)

    if (keepAlive) nettyResponse.setHeader(Names.CONTENT_LENGTH, nettyResponse.getContent().readableBytes())

    val future = e.getChannel().write(nettyResponse)

    if (!keepAlive) future.addListener(ChannelFutureListener.CLOSE)
  }
}

class HttpServerPipelineFactory(val requestChannelHandler: ChannelHandler) extends ChannelPipelineFactory {
  def getPipeline(): ChannelPipeline = {
    val pipeline = Channels.pipeline()

    pipeline.addLast("decoder", new HttpRequestDecoder())
    pipeline.addLast("encoder", new HttpResponseEncoder())

    pipeline.addLast("aggregator", new HttpChunkAggregator(1048576));

    pipeline.addLast("handler", requestChannelHandler)

    pipeline
  }
}

trait NettyEngineArrayByte extends NettyEngine[Array[Byte]]{ self: HttpServer[Array[Byte]] =>
  implicit val contentBijection = NettyBijections.ChannelBufferToByteArray
}

trait NettyEngineString extends NettyEngine[String]{ self: HttpServer[String] =>
  implicit val contentBijection = NettyBijections.ChannelBufferToString
}

object NettyBijections{
  val ChannelBufferToByteArray = new Bijection[ChannelBuffer, Array[Byte]]{
    def apply(content: ChannelBuffer) = content.array()
    def unapply(content: Array[Byte]) = ChannelBuffers.copiedBuffer(content)
  }

  val ChannelBufferToString = new Bijection[ChannelBuffer, String]{
    def apply(content: ChannelBuffer) = content.toString(CharsetUtil.UTF_8)
    def unapply(content: String)      = ChannelBuffers.copiedBuffer(content, CharsetUtil.UTF_8)
  }
}
//  private def encodeCookie(request: NettyHttpRequest, response: NettyHttpResponse) = {
//    val cookieString = request.getHeader(Names.COOKIE)
//    if (cookieString != null) {
//      val cookieDecoder = new CookieDecoder()
//      val cookieEncoder = new CookieEncoder(true)
//      val cookies       = cookieDecoder.decode(cookieString)
//      cookies.foreach(cookieEncoder.addCookie(_))
//      response.addHeader(Names.SET_COOKIE, cookieEncoder.encode())
//    }
//  }
