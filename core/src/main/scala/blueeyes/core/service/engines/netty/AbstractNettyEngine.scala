package blueeyes.core.service
package engines.netty

import blueeyes.bkka._
import blueeyes.core.data._
import akka.dispatch.Future
import akka.dispatch.ExecutionContext

import com.weiglewilczek.slf4s.Logging
import com.weiglewilczek.slf4s.Logger
import org.streum.configrity.Configuration


trait AbstractNettyEngine extends HttpServerModule { self =>
  type HttpServer <: AbstractNettyHttpServer

  abstract class AbstractNettyHttpServer(rootConfig: Configuration, services: List[Service[ByteChunk, _]], executor0: ExecutionContext) extends HttpServerLike(rootConfig, services, executor0) {

    override def start = {
      implicit val stop: Stop[List[NettyServer]] = new Stop[List[NettyServer]] {
        def stop(servers: List[NettyServer]) = Future(servers.foreach(_.stop))
      }

      super.start.map { service =>
        for {
          (service, stoppable) <- service
          servers = nettyServers(service)
          _ <- Future(servers.foreach(_.start))
        } yield {
          (service, Some(Stoppable(servers, stoppable.toList)))
        }
      }
    }

    protected def nettyServers(service: AsyncHttpService[ByteChunk]): List[NettyServer]
  }
}
