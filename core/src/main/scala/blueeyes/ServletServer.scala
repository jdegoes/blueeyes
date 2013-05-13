package blueeyes

import core.data.ByteChunk
import core.service._
import core.service.engines.servlet.ServletEngine

import akka.dispatch.ExecutionContext

import org.streum.configrity.Configuration
import com.weiglewilczek.slf4s.Logging

/** Convenience trait for building a servlet. This server uses reflection to mix
 * in any services defined as fields.
 * <pre>
 * trait EmailServices extends BlueEyesServiceBuilder {
 *   val emailService = service("email", "1.32") {
 *     request {
 *       ...
 *     }
 *   }
 * }
 * object EmailServer extends ServletServer with EmailServices
 * </pre>
 */

trait ServletServer extends ServletEngine with ReflectiveServiceList[ByteChunk] { self =>
  type HttpServer = HttpServerLike
  
  def server(rootConfig: Configuration, ctx: ExecutionContext) = 
    new HttpServerLike(rootConfig, services, ctx)
}
