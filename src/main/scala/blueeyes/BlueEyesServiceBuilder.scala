package blueeyes

import blueeyes.core.http._
import blueeyes.core.data.BijectionsByteArray
import blueeyes.core.service._
import blueeyes.util.{Future, FutureImplicits}

/** Convenience trait for building services with many common mixins. 
 * <pre>
 * services {
 *   email {
 *     v1 {
 *       ...
 *       
 *       log {
 *       
 *       }
 *     }
 *   }
 * }
 * </pre>
 * <pre>
 * trait EmailServices extends BlueEyesServiceBuilder {
 *   case class EmailState(contacts: List[String])
 *
 *   val emailService = service("email", "1.32") { context =>
 *     startup {
 *       val file = context.config.getString("contactsFileName")
 *
 *       Future.async {
 *         context.log.info("Attempting to load contacts list...")
 *
 *         EmailState(load(file))
 *       }
 *     } ->
 *     request { state =>
 *       path("/foo") {
 *          get { request =>
 *
 *          } ~
 *
 *          post { request =>
 *
 *          }
 *       } ~ 
 *       
 *       path("/bar") {
 *
 *       }
 *     } ->
 *     shutdown { state =>
 *        
 *     }
 *   }
 * }
 * object EmailServer extends BlueEyesServer with EmailServices
 * </pre>
 */
trait BlueEyesServiceBuilder extends HttpServiceBuilder[Array[Byte]] with 
  FutureImplicits with 
  HttpHeaderImplicits with 
  HttpStatusImplicits with
  HttpStatusCodeImplicits with 
  HttpDateImplicits with
  HttpNumberImplicits with
  HttpRequestHandlerCombinators with 
  HttpRequestHandlerImplicits with
  RestPathPatternImplicits with
  BijectionsByteArray {
  
}