package blueeyes

import core.data.ByteChunk
import core.service._
import core.service.engines.netty.NettyEngine

import akka.dispatch.ExecutionContext

import org.streum.configrity.Configuration
import com.weiglewilczek.slf4s.Logging

/** Convenience trait for building a server. This server uses reflection to mix
 * in any services defined as fields.
 * <pre>
 * trait EmailServices extends BlueEyesServiceBuilder {
 *   val emailService = service("email", "1.32") {
 *     request {
 *       ...
 *     }
 *   }
 * }
 * object EmailServer extends BlueEyesServer with EmailServices
 * </pre>
 */
trait BlueEyesServer extends NettyEngine with ReflectiveServiceList[ByteChunk] with Logging { self =>
  class HttpServer(rootConfig: Configuration, ctx: ExecutionContext) extends NettyHttpServer(rootConfig, ctx) {
    implicit val executionContext = ctx
    val services = self.services
    val logger = self.logger
  }

  def server(rootConfig: Configuration, ctx: ExecutionContext) = new HttpServer(rootConfig, ctx)
}
