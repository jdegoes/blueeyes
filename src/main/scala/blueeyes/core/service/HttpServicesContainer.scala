package blueeyes.core.service

import net.lag.configgy.Config

/**
 * A container for services. At a minimum, containers must define the root 
 * configuration object used by all services.
 */
trait HttpServicesContainer[Base] {
  import scala.collection.mutable.{Buffer, ListBuffer}
  
  def rootConfig: Config
  
  final val services: Buffer[HttpService2[Base]] = new ListBuffer[HttpService2[Base]]
}