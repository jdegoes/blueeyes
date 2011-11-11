package blueeyes.core.service

import org.specs2.mutable.Specification

class HttpServiceVersionImplicitsSpec extends Specification{

  "HttpServiceVersionImplicits stringToVersion: creates version" in{
    ServiceVersion.fromString("1.2.3") mustEqual(ServiceVersion(1, 2, "3"))
  }
}
