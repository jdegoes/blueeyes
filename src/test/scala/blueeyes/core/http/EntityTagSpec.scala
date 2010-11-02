package blueeyes.core.http

import org.specs.Specification
import blueeyes.core.http.MimeTypes._

class HttpHeadersSpec extends Specification {

  /* Some more work should be done on entity tags */
  "If-Match:  Should return strings on well-formed input" in {
    HttpHeaders.`If-Match`(EntityTags.parseEntityTags("\"c4tattack\", \"cyberTiger\"").get).value mustEqual "\"c4tattack\", \"cybertiger\""
    }

  "If-Match:  Should return * string on presence of *; also, parser" in {
    HttpHeaders.`If-Match`(EntityTags.parseEntityTags("*, \"c4tattack\", \"cyberTiger\"").get).value mustEqual "*"
  }

  "If-Match: Should return none text not enclosed with quotes" in {
    EntityTags.parseEntityTags("w%015") mustEqual None
  }

}
