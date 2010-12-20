package blueeyes.core.service.engines

import blueeyes.core.service._
import org.jboss.netty.util.CharsetUtil
import blueeyes.core.data.Bijection
import org.jboss.netty.buffer.{ChannelBuffers, ChannelBuffer}
import blueeyes.util.Future
import org.jboss.netty.bootstrap.ServerBootstrap
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import java.util.concurrent.{Executors, Executor}
import org.jboss.netty.channel._
import org.jboss.netty.util.internal.ExecutorUtil
import java.io.ByteArrayOutputStream
import java.net.{InetAddress, InetSocketAddress}
import net.lag.configgy.ConfigMap
import org.jboss.netty.handler.codec.http.{HttpContentCompressor, HttpChunkAggregator, HttpResponseEncoder, HttpRequestDecoder}
import org.jboss.netty.handler.ssl.SslHandler
import security.BlueEyesKeyStoreFactory
import util.matching.Regex

trait NettyEngine[T] extends HttpServerEngine[T] with HttpServer[T]{ self =>

  private val startStopLock = new java.util.concurrent.locks.ReentrantReadWriteLock

  private var servers: List[Tuple2[Executor, ServerBootstrap]]  = Nil

  override def start: Future[Unit] = {
    super.start.flatMapEither(_ => {

      startStopLock.writeLock.lock()

      try {
        val host = InetInrerfaceLookup.host(config)

        try {
          startServers(List(Tuple2(port, new HttpServerPipelineFactory("http", host, port)), Tuple2(sslPort, new HttpsServerPipelineFactory("https", host, sslPort))))

          log.info("Http Netty engine is started using port: " + self.port)
          log.info("Https Netty engine is started using port: " + self.sslPort)
          Right(())
        }
        catch {
          case e: Throwable => {
            log.error(e, "Error while servers start: %s", e.getMessage)
            stop
            Left(e)
          }
        }
      }
      finally{
        startStopLock.writeLock.unlock()
      }
    })
  }

  private def startServers(serversArgs: List[Tuple2[Int, ChannelPipelineFactory]]){
    serversArgs.foreach(serverArg => {
      try {
        val executor = Executors.newCachedThreadPool()
        val bootstrap = new ServerBootstrap(new NioServerSocketChannelFactory(executor, executor))

        bootstrap.setPipelineFactory(serverArg._2)

        bootstrap.bind(InetInrerfaceLookup.socketAddres(config, serverArg._1))

        servers = Tuple2(executor, bootstrap) :: servers
      }
    })
  }

  override def stop: Future[Unit] = {
    super.stop.map(_ => {

      startStopLock.writeLock.lock()
      
      try {
        servers.foreach(server => {
          ExecutorUtil.terminate(server._1)
          server._2.releaseExternalResources
        })
        servers = Nil
      }
      finally{
        startStopLock.writeLock.unlock()
      }

      log.info("Netty engine is stopped.")
      ()
    })
  }

  implicit def contentBijection: Bijection[ChannelBuffer, T]

  class HttpsServerPipelineFactory(protocol: String, host: String, port: Int) extends HttpServerPipelineFactory(protocol: String, host, port) {
    private val keyStore = BlueEyesKeyStoreFactory(config)

    override def getPipeline(): ChannelPipeline = {
      val pipeline = super.getPipeline

      val engine = SslContextFactory(keyStore, BlueEyesKeyStoreFactory.password).createSSLEngine();
      engine.setUseClientMode(false);
      pipeline.addFirst("ssl", new SslHandler(engine))

      pipeline
    }
  }

  class HttpServerPipelineFactory(protocol: String, host: String, port: Int) extends ChannelPipelineFactory {
    def getPipeline(): ChannelPipeline = {
      val pipeline     = Channels.pipeline()

      pipeline.addLast("decoder",     new FullURIHttpRequestDecoder(protocol, host, port))
      pipeline.addLast("aggregator",  new HttpChunkAggregator(1048576));
      pipeline.addLast("encoder",     new HttpResponseEncoder())
      pipeline.addLast("deflater",    new HttpContentCompressor())

      pipeline.addLast("handler", new NettyRequestHandler[T](self, log))

      pipeline
    }
  }
}

private[engines] object InetInrerfaceLookup{
  def socketAddres(config: ConfigMap, port: Int) = config.getString("address").map(v => new InetSocketAddress(v, port)).getOrElse(new InetSocketAddress(port))

  def host(config: ConfigMap) = config.getString("address").getOrElse(InetAddress.getLocalHost().getHostName())
}

private[engines] class FullURIHttpRequestDecoder(protocol: String, host: String, port: Int) extends HttpRequestDecoder{
  private val baseUri = """%s://%s:%d""".format(protocol, host, port)
  private val fullUriRegexp = new Regex("""(https|http)://.+/(:\d+/)?.+""")
  override def createMessage(initialLine: Array[String]) = {
    val path = initialLine(1)
    if (!fullUriRegexp.pattern.matcher(path).matches) initialLine(1) = baseUri + (if (path.startsWith("/")) path else "/" + path)
    super.createMessage(initialLine)
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
    def apply(content: ChannelBuffer) = {
      val stream = new ByteArrayOutputStream()
      try {
        content.readBytes(stream, content.readableBytes)
        stream.toByteArray
      }
      finally stream.close
    }
    def unapply(content: Array[Byte]) = ChannelBuffers.copiedBuffer(content)
  }

  val ChannelBufferToString = new Bijection[ChannelBuffer, String]{
    def apply(content: ChannelBuffer) = new String(ChannelBufferToByteArray.apply(content), CharsetUtil.UTF_8) 
    def unapply(content: String)      = ChannelBuffers.copiedBuffer(content, CharsetUtil.UTF_8)
  }
}
