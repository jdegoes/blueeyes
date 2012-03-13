package blueeyes.core.service.engines

import org.specs2.mutable.Specification
import org.streum.configrity.Configuration 
import org.streum.configrity.io.BlockFormat
import java.net.{InetSocketAddress, InetAddress}

class InetInterfaceLookupSpec extends Specification{
  override def is = args(sequential = true) ^ super.is
  "creates socket address when address is not configured" in{
    val config = Configuration.parse("", BlockFormat)

    InetInterfaceLookup.socketAddres(config.detach("server"), 8080) mustEqual(new InetSocketAddress(8080))
  }
  "creates host name when address is not configured" in{
    val config = Configuration.parse("", BlockFormat)

    InetInterfaceLookup.host(config.detach("server")) mustEqual(InetAddress.getLocalHost().getHostName())
  }
  "creates socket address when address is configured" in{
    val rawConfig = """
    server {
      address = 192.168.10.10
    }
    """

    val config = Configuration.parse(rawConfig, BlockFormat)

    InetInterfaceLookup.socketAddres(config.detach("server"), 8080) mustEqual(new InetSocketAddress("192.168.10.10", 8080))
  }
  "creates host name when address is configured" in{
    val rawConfig = """
    server {
      address = 192.168.10.10
    }
    """
    val config = Configuration.parse(rawConfig, BlockFormat)

    InetInterfaceLookup.host(config.detach("server")) mustEqual("192.168.10.10")
  }
}
