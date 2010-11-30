package blueeyes.core.service.engines

import org.specs.Specification
import net.lag.configgy.Configgy
import java.net.{InetSocketAddress, InetAddress}

class InetInrerfaceLookupSpec extends Specification{
  "creates socket address when address is not configured" in{
    Configgy.configureFromString("")

    InetInrerfaceLookup.socketAddres(Configgy.config.configMap("server"), 8080) mustEqual(new InetSocketAddress(8080))
  }
  "creates host name when address is not configured" in{
    Configgy.configureFromString("")

    InetInrerfaceLookup.host(Configgy.config.configMap("server")) mustEqual(InetAddress.getLocalHost().getHostName())
  }
  "creates socket address when address is configured" in{
    Configgy.configureFromString("""server{address="192.168.10.10"}""")

    InetInrerfaceLookup.socketAddres(Configgy.config.configMap("server"), 8080) mustEqual(new InetSocketAddress("192.168.10.10", 8080))
  }
  "creates host name when address is configured" in{
    Configgy.configureFromString("""server{address="192.168.10.10"}""")

    InetInrerfaceLookup.host(Configgy.config.configMap("server")) mustEqual("192.168.10.10")
  }
}