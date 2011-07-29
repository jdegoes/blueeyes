package blueeyes.core.service.engines

import blueeyes.core.service._
import blueeyes.concurrent.Future
import org.jboss.netty.bootstrap.ServerBootstrap
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import java.util.concurrent.{Executors, Executor}
import org.jboss.netty.channel._
import org.jboss.netty.util.internal.ExecutorUtil
import java.net.{InetAddress, InetSocketAddress}
import net.lag.configgy.ConfigMap
import org.jboss.netty.handler.codec.http.{HttpResponseEncoder, HttpRequestDecoder}
import org.jboss.netty.handler.ssl.SslHandler
import security.BlueEyesKeyStoreFactory
import util.matching.Regex
import net.lag.logging.Logger
import org.jboss.netty.handler.stream.ChunkedWriteHandler

trait NettyEngine extends HttpServerEngine with HttpServer{ self =>

  private val startStopLock = new java.util.concurrent.locks.ReentrantReadWriteLock

  private var servers: List[Tuple2[Executor, ServerBootstrap]]  = Nil

  override def start: Future[Unit] = {
    super.start.flatMapEither(_ => {

      startStopLock.writeLock.lock()

      try {
        try {
          val servers = Tuple2(port, new HttpServerPipelineFactory("http", host, port, chunkSize)) :: (if (sslEnable) Tuple2(sslPort, new HttpsServerPipelineFactory("https", host, sslPort, chunkSize)) :: Nil else Nil)

          startServers(servers)

          servers.foreach(server => log.info("%s netty engine is started using port: %d".format(server._2.protocol, server._1)))

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
        bootstrap.setParentHandler(new SetBacklogHandler(config.getInt("backlog", 10000)))
        bootstrap.setPipelineFactory(serverArg._2)

        bootstrap.bind(InetInterfaceLookup.socketAddres(config, serverArg._1))

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
          server._2.releaseExternalResources()
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

  class HttpServerPipelineFactory(val protocol: String, host: String, port: Int, chunkSize: Int) extends ChannelPipelineFactory {
    def getPipeline: ChannelPipeline = {
      val pipeline = Channels.pipeline()

      pipeline.addLast("decoder",       new FullURIHttpRequestDecoder(protocol, host, port, chunkSize))
      pipeline.addLast("encoder",       new HttpResponseEncoder())
      pipeline.addLast("chunkedWriter", new ChunkedWriteHandler())
      pipeline.addLast("aggregator",    new NettyChunkedRequestHandler(chunkSize))
      pipeline.addLast("handler",       new NettyRequestHandler(self, Logger.get))

      pipeline
    }
  }

  class HttpsServerPipelineFactory(protocol: String, host: String, port: Int, chunkSize: Int) extends HttpServerPipelineFactory(protocol: String, host, port, chunkSize) {
    private val keyStore = BlueEyesKeyStoreFactory(config)

    override def getPipeline: ChannelPipeline = {
      val pipeline = super.getPipeline()

      val engine = SslContextFactory(keyStore, BlueEyesKeyStoreFactory.password).createSSLEngine()
      
      engine.setUseClientMode(false);
      
      pipeline.addFirst("ssl", new SslHandler(engine))

      pipeline
    }
  }
}

private[engines] class SetBacklogHandler(backlog: Int) extends SimpleChannelUpstreamHandler{
  override def channelOpen(ctx: ChannelHandlerContext, e: ChannelStateEvent) {
    e.getChannel.getConfig.setOption("backlog", backlog)
    super.channelOpen(ctx, e)
  }
}

private[engines] object InetInterfaceLookup {
  def socketAddres(config: ConfigMap, port: Int) = config.getString("address").map(v => new InetSocketAddress(v, port)).getOrElse(new InetSocketAddress(port))

  def host(config: ConfigMap) = config.getString("address").getOrElse(InetAddress.getLocalHost.getHostName)
}

private[engines] class FullURIHttpRequestDecoder(protocol: String, host: String, port: Int, chunkSize: Int) extends HttpRequestDecoder(4096, 8192, 2){
  private val baseUri = """%s://%s:%d""".format(protocol, host, port)
  private val fullUriRegexp = new Regex("""(https|http)://.+/(:\d+/)?.+""")
  override def createMessage(initialLine: Array[String]) = {
    val path = initialLine(1)
    if (!fullUriRegexp.pattern.matcher(path).matches) initialLine(1) = baseUri + (if (path.startsWith("/")) path else "/" + path)
    super.createMessage(initialLine)
  }
}