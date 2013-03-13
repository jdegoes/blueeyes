package blueeyes.core.http

import org.specs2.mutable.Specification
import HttpHeaders._

class HttpHeadersSpec extends Specification{
  "HttpHeaders" should {
    "find headers by type" in{
      val headers = HttpHeaders(List("authorization" -> "foo"))
      headers.header[Authorization] must beSome(Authorization("foo"))
    }

    "maintain case-insensitivity when appending tuple" in {
      val headers = HttpHeaders.Empty + ("authorization" -> "foo")
      headers.header[Authorization] must beSome(Authorization("foo"))
    }

    "extract known header types" in {
      val headers = Map("authorization" -> "foo")

      val auths = for (Authorization(auth) <- headers) yield auth
      auths must_== List("foo")
    }
    "extract Host header types without scheme" in {
      val headers = Map("Host" -> "localhost:8585")

      val hosts = for (Host(host) <- headers) yield host
      hosts.map(Host(_).value) must_== List("localhost:8585")
    }

    "parse custom tuples" in {
      HttpHeader(("Blargh" -> "foo")) must_== CustomHeader("Blargh", "foo")
    }
  }

  // CORS headers:

  "Access-Control-Max-Age" should {
    "accept an long" in {
      `Access-Control-Max-Age`(123L).value mustEqual "123"
    }
    "be parseable" in {
      HttpHeader(("Access-Control-Max-Age", "123")) mustEqual `Access-Control-Max-Age`(123L)
      HttpHeader(("access-control-max-age", "234")) mustEqual `Access-Control-Max-Age`(234L)
    }
  }

  "Access-Control-Allow-Credentials" should {
    "accept a boolean" in {
      `Access-Control-Allow-Credentials`(true).value mustEqual "true"
      `Access-Control-Allow-Credentials`(false).value mustEqual "false"
    }
    "be parseable" in {
      HttpHeader(("Access-Control-Allow-Credentials", "true")) mustEqual `Access-Control-Allow-Credentials`(true)
      HttpHeader(("access-control-allow-credentials", "FALse ")) mustEqual `Access-Control-Allow-Credentials`(false)
    }
  }

  "Access-Control-Allow-Methods" should {
    import HttpMethods._
    "accept methods" in {
      `Access-Control-Allow-Methods`(GET).value mustEqual "GET"
      `Access-Control-Allow-Methods`(GET, POST).value mustEqual "GET,POST"
    }
    "be parseable" in {
      HttpHeader(("Access-Control-Allow-Methods", "GET")) mustEqual `Access-Control-Allow-Methods`(GET)
      HttpHeader(("access-control-allow-methods", "Get , post ")) mustEqual `Access-Control-Allow-Methods`(GET, POST)
    }
  }

  "Access-Control-Allow-Headers" should {
    "accept headers" in {
      `Access-Control-Allow-Headers`(`Content-Type`).value mustEqual "Content-Type"
      `Access-Control-Allow-Headers`(`Content-Type`, Authorization).value mustEqual "Content-Type,Authorization"
    }
    "be parseable" in {
      HttpHeader(("Access-Control-Allow-Headers", "content-type")) mustEqual `Access-Control-Allow-Headers`(`Content-Type`)
      HttpHeader(("access-control-allow-headers", "content-type , AUTHORIZATION ")) mustEqual `Access-Control-Allow-Headers`(`Content-Type`, Authorization)
    }
  }

  "Origin" should {
    "accept a boolean" in {
      Origin("example.com").value mustEqual "example.com"
    }
    "be parseable" in {
      HttpHeader(("Origin", "example.com")) mustEqual Origin("example.com")
      HttpHeader(("origin", "example.com")) mustEqual Origin("example.com")
    }
  }
}
