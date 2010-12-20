package blueeyes.core.service

import net.lag.configgy.ConfigMap

case class HttpServiceContext(config: ConfigMap, serviceName: String, serviceVersion: HttpServiceVersion)