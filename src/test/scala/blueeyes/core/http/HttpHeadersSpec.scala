package blueeyes.core.http

import org.specs.Specification

class HttpHeadersSpec extends Specification {
  "HttpHeaders: headerOption return header by class" in{
    val headers = new HttpHeaders(Map("authorization" -> "foo"))

    headers.headerOption[HttpHeaders.Authorization] must beSome(HttpHeaders.Authorization("foo"))
  }
  "HttpHeaders: header return header by class" in{
    val headers = new HttpHeaders(Map("authorization" -> "foo"))

    headers.header[HttpHeaders.Authorization] mustEqual(HttpHeaders.Authorization("foo"))
  }
  "HttpHeaders: header throws error when header is missing" in{
    val headers = new HttpHeaders(Map())

    headers.header[HttpHeaders.Authorization] must throwA[RuntimeException]
  }
}