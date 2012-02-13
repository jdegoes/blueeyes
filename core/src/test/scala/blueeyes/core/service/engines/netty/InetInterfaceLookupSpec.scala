package blueeyes.core.service.engines.netty

import _root_..
import _root_..
import _root_..
import _root_..
import org.specs2.mutable.Specification
import net.lag.configgy.Configgy
import java.net.{InetSocketAddress, InetAddress}

class InetInterfaceLookupSpec extends Specification{
  override def is = args(sequential = true) ^ super.is
  "creates socket address when address is not configured" in{
    Configgy.configureFromString("")

    InetInterfaceLookup.socketAddres(Configgy.config.configMap("server"), 8080) mustEqual(new InetSocketAddress(8080))
  }
  "creates host name when address is not configured" in{
    Configgy.configureFromString("")

    InetInterfaceLookup.host(Configgy.config.configMap("server")) mustEqual(InetAddress.getLocalHost().getHostName())
  }
  "creates socket address when address is configured" in{
    Configgy.configureFromString("""server{address="192.168.10.10"}""")

    InetInterfaceLookup.socketAddres(Configgy.config.configMap("server"), 8080) mustEqual(new InetSocketAddress("192.168.10.10", 8080))
  }
  "creates host name when address is configured" in{
    Configgy.configureFromString("""server{address="192.168.10.10"}""")

    InetInterfaceLookup.host(Configgy.config.configMap("server")) mustEqual("192.168.10.10")
  }
}