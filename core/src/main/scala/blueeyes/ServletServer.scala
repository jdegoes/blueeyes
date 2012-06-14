package blueeyes

import core.data.ByteChunk
import core.service.engines.servlet.ServletEngine
import core.service.{HttpServer, HttpReflectiveServiceList}

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

trait ServletServer extends ServletEngine with HttpServer with HttpReflectiveServiceList[ByteChunk]