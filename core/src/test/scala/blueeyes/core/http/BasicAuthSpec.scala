package blueeyes.core.http

import org.apache.commons.codec.binary.Base64
import org.specs2.mutable.Specification

class BasicAuthSpec extends Specification {
  "parsing a basic auth credentials header" should {
    "correctly recover username:password" in {
      val encoded = new String(Base64.encodeBase64("myUser:myPass".getBytes("UTF-8")), "UTF-8")
      val creds = "Basic " + encoded
      
      BasicAuthCredentialsParser.parse(creds) must beSome(BasicAuthCredentials("myUser", "myPass"))
    }
  }
}


// vim: set ts=4 sw=4 et:
