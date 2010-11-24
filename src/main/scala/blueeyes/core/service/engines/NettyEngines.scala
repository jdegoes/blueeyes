package blueeyes.core.service.engines

import blueeyes.core.service._
import org.jboss.netty.util.CharsetUtil
import blueeyes.core.data.Bijection
import java.lang.String
import org.jboss.netty.buffer.{ChannelBuffers, ChannelBuffer}
import blueeyes.util.Future
import org.jboss.netty.bootstrap.ServerBootstrap
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import java.util.concurrent.{Executors, Executor}
import org.jboss.netty.channel._
import org.jboss.netty.util.internal.ExecutorUtil
import org.jboss.netty.handler.codec.http.{HttpChunkAggregator, HttpResponseEncoder, HttpRequestDecoder}
import java.io.ByteArrayOutputStream
import java.net.{InetAddress, InetSocketAddress}
import net.lag.configgy.ConfigMap

trait NettyEngine[T] extends HttpServerEngine[T] with HttpServer[T]{ self =>

  private val startStopLock = new java.util.concurrent.locks.ReentrantReadWriteLock

  private var server: Option[ServerBootstrap]  = None
  private var serverExecutor: Option[Executor] = None

  override def start: Future[Unit] = {
    super.start.map(_ => {

      startStopLock.writeLock.lock()

      try {
        val executor     = Executors.newCachedThreadPool()
        val bootstrap    = new ServerBootstrap(new NioServerSocketChannelFactory(executor, executor))
        val inteIterface = InetInrerfaceLookup(config, port)

        bootstrap.setPipelineFactory(new HttpServerPipelineFactory(inteIterface._2, self.port))

        bootstrap.bind(inteIterface._1)

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

  class HttpServerPipelineFactory(host: String, port: Int) extends ChannelPipelineFactory {
    def getPipeline(): ChannelPipeline = {
      val pipeline     = Channels.pipeline()

      pipeline.addLast("decoder", new FullURIHttpRequestDecoder("http", host, port))
      pipeline.addLast("encoder", new HttpResponseEncoder())

      pipeline.addLast("aggregator", new HttpChunkAggregator(1048576));

      pipeline.addLast("handler", new NettyRequestHandler[T](self, log))

      pipeline
    }
  }
  
}

private[engines] object InetInrerfaceLookup{
  def apply(config: ConfigMap, port: Int) = {
    config.getString("address").map(v => Tuple2(new InetSocketAddress(v, port), v)).getOrElse(Tuple2(new InetSocketAddress(port), InetAddress.getLocalHost().getHostName()))
  }
}

private[engines] class FullURIHttpRequestDecoder(protocol: String, host: String, port: Int) extends HttpRequestDecoder{
  private val baseUri = """%s://%s:%d""".format(protocol, host, port)
  override def createMessage(initialLine: Array[String]) = {
    val path = initialLine(1)
    initialLine(1) = baseUri + (if (path.startsWith("/")) path else "/" + path)
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