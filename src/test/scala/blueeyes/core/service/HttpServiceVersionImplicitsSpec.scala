package blueeyes.core.service

import org.spex.Specification

class HttpServiceVersionImplicitsSpec extends Specification{

  "HttpServiceVersionImplicits stringToVersion: creates version" in{
    HttpServiceVersionImplicits.stringToVersion("1.2.3") mustEqual(HttpServiceVersion(1, 2, "3"))
  }
}