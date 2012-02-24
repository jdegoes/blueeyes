package blueeyes.core.service

import org.streum.configrity.Configuration 

case class ServiceContext(rootConfig: Configuration, config: Configuration, serviceName: String, serviceVersion: ServiceVersion, desc: Option[String], hostName: String, port: Int, sslPort: Int) {
  override def toString = serviceName + ".v" + serviceVersion.majorVersion
}
