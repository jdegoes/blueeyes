package blueeyes.core.service

import net.lag.configgy.ConfigMap

case class HttpServiceContext(config: ConfigMap, serviceName: String, serviceVersion: HttpServiceVersion, hostName: String, port: Int, sslPort: Int) {
  override def toString = serviceName + ".v" + serviceVersion.majorVersion
}