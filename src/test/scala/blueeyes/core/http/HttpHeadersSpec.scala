package blueeyes.core.http

import org.specs.Specification
import HttpHeaders._

class HttpHeadersSpec extends Specification {
  "HttpHeaders" should {
    "find headers by type" in{
      val headers = HttpHeaders(List("authorization" -> "foo"))
      headers.header[Authorization] must beSome(Authorization("foo"))
    }

    "extract known header types" in {
      val headers = Map("authorization" -> "foo")

      val auths = for (Authorization(auth) <- headers) yield auth
      auths must_== List("foo") 
    }

    "parse custom tuples" in {
      val headers = 

      HttpHeader(("Blargh" -> "foo")) must_== CustomHeader("Blargh", "foo")
    }
  }
}
