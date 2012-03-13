package blueeyes.core.service.engines

import org.streum.configrity.Configuration
import java.net.{InetAddress, InetSocketAddress}

object InetInterfaceLookup {
  def socketAddres(config: Configuration, port: Int) = config.get[String]("address").map(v => new InetSocketAddress(v, port)).getOrElse(new InetSocketAddress(port))

  def host(config: Configuration) = config.get[String]("address").getOrElse(InetAddress.getLocalHost.getHostName)
}
