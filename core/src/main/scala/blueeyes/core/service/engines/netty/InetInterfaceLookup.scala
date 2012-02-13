package blueeyes.core.service.engines.netty

import net.lag.configgy.ConfigMap
import java.net.{InetAddress, InetSocketAddress}

object InetInterfaceLookup {
  def socketAddres(config: ConfigMap, port: Int) = config.getString("address").map(v => new InetSocketAddress(v, port)).getOrElse(new InetSocketAddress(port))

  def host(config: ConfigMap) = config.getString("address").getOrElse(InetAddress.getLocalHost.getHostName)
}
