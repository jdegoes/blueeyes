package blueeyes.core.service

import net.lag.configgy.Config

/**
 * A container for services.
 */
trait HttpServicesContainer[Base] {
  def rootConfig: Config
  
  def services: List[HttpService2[Base]]
}