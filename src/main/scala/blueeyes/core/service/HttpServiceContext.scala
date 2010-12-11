package blueeyes.core.service

import net.lag.configgy.ConfigMap
import net.lag.logging.Logger
import blueeyes.health.HealthMonitor

case class HttpServiceContext(config: ConfigMap, log: Logger, healthMonitor: HealthMonitor)