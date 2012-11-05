package blueeyes.core.service
package engines.netty

import blueeyes.core.data._

import akka.dispatch.ExecutionContext
import org.streum.configrity.Configuration

trait NettyEngine extends AbstractNettyEngine { self =>
  type HttpServer <: NettyHttpServer

  abstract class NettyHttpServer(rootConfig: Configuration, executionContext: ExecutionContext) extends AbstractNettyHttpServer(rootConfig) { server =>
    protected def nettyServers(service: AsyncHttpService[ByteChunk]) = {
      val httpProvider = new HttpNettyServerProvider(server, service, executionContext)
      val httpServer = new NettyServer(httpProvider)
      if (server.sslEnable) {
        val httpsProvider = new HttpsNettyServerProvider(server, service, executionContext)
        val httpsServer = new NettyServer(httpsProvider)

        httpServer :: httpsServer :: Nil
      } else {
        httpServer :: Nil
      }
    }
  }
}
