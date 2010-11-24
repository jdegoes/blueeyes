package blueeyes.core.service.engines

import org.specs.Specification
import net.lag.configgy.Configgy
import java.net.{InetSocketAddress, InetAddress}

class InetInrerfaceLookupSpec extends Specification{
  "creates interface when address is not configured" in{
    Configgy.configureFromString("")

    InetInrerfaceLookup(Configgy.config.configMap("server"), 8080) mustEqual(Tuple2(new InetSocketAddress(8080), InetAddress.getLocalHost().getHostName()))
  }
  "creates interface when address is configured" in{
    Configgy.configureFromString("""server{address="192.168.10.10"}""")

    InetInrerfaceLookup(Configgy.config.configMap("server"), 8080) mustEqual(Tuple2(new InetSocketAddress("192.168.10.10", 8080), "192.168.10.10"))
  }
}