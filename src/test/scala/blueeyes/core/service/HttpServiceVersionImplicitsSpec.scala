package blueeyes.core.service

import org.specs2.mutable.Specification
import org.specs2.matcher.MustThrownMatchers

class HttpServiceVersionImplicitsSpec extends Specification with MustThrownMatchers{

  "HttpServiceVersionImplicits stringToVersion: creates version" in{
    ServiceVersion.fromString("1.2.3") mustEqual(ServiceVersion(1, 2, "3"))
  }
}
