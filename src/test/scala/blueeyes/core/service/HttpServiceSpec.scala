package blueeyes.core.service

import org.spex.Specification

class HttpServiceSpec extends Specification{

  "HttpService: creates majorVersion and minorVersion based in version" in{
    service.majorVersion must be (1)

    service.minorVersion must be (2)
  }

  private val service = new HttpService[Unit]{
    def ioClass = null

    def descriptorFactory = null

    def version = "1.2.3"

    def name = null
  }
}