package blueeyes

import blueeyes.core.http._
import blueeyes.core.service._
import blueeyes.util.{Future, FutureImplicits}

/** Convenience trait for building services with many common mixins. 
 */
trait BlueEyesServiceBuilder[T] extends HttpServiceBuilder[T] with 
  FutureImplicits with 
  HttpHeaderImplicits with 
  HttpStatusImplicits with
  HttpStatusCodeImplicits with 
  HttpDateImplicits with
  HttpNumberImplicits with
  HttpRequestHandlerCombinators with 
  RestPathPatternImplicits {
  
}