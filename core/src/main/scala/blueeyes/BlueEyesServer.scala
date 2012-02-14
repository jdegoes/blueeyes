package blueeyes

import blueeyes.core.service._
import blueeyes.core.service.engines._
import core.data.ByteChunk
import netty.NettyEngine

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
trait BlueEyesServer extends HttpServer with HttpReflectiveServiceList[ByteChunk] with NettyEngine {

}