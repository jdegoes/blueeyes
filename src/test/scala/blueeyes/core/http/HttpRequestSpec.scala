package blueeyes.core.http

import org.specs.Specification

class HttpRequestSpec extends Specification {
  "HttpRequest.apply" should {
    "parse query string properly" in {
      val ps = HttpRequest(method = HttpMethods.GET, uri = "http://foo.com?a=b&c=d&e").parameters

      ps mustEqual Map(
        'a -> "b",
        'c -> "d",
        'e -> ""
      )
    }

    "override parameters with query string parameters" in {
      val ps = HttpRequest(method = HttpMethods.GET, uri = "http://foo.com?a=b", parameters = Map('a -> "z")).parameters

      ps mustEqual Map(
        'a -> "b"
      )
    }

    "not doubly unescape query parameters" in {
      val ps = HttpRequest(method = HttpMethods.GET, uri = "http://foo.com?a=a%26b%26c%26d").parameters

      ps mustEqual Map(
        'a -> "a&b&c&d"
      )
    }

    "allow no query string" in {
      val ps = HttpRequest(method = HttpMethods.GET, uri = "http://foo.com").parameters

      ps mustEqual Map.empty[Symbol, String]
    }

    "allow empty query string" in {
      val ps = HttpRequest(method = HttpMethods.GET, uri = "http://foo.com?").parameters

      ps mustEqual Map.empty[Symbol, String]
    }
  }
}
