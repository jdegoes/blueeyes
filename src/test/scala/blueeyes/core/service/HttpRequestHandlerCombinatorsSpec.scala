package blueeyes.core.service

import org.specs.Specification
import org.specs.matcher.Matchers._

import blueeyes.core.http._
import blueeyes.core.http.MimeType
import blueeyes.core.http.MimeTypes._
import blueeyes.core.data.BijectionsString._
import blueeyes.json.JsonAST._
import blueeyes.concurrent.Future
import blueeyes.concurrent.FutureImplicits

import java.net.URLDecoder.{decode => decodeUrl}
import java.net.URLEncoder.{encode => encodeUrl}

class HttpRequestHandlerCombinatorsSpec extends Specification with HttpRequestHandlerCombinators with RestPathPatternImplicits with HttpRequestHandlerImplicits{
  "composition of paths" should {
    "have the right type" in {
      val handler: HttpRequestHandler[Int] = {
        path("/foo/bar") {
          path("/baz") {
            get { (request: HttpRequest[Int]) =>
              Future(HttpResponse[Int]())
            }
          }
        }
      }

      handler mustBe handler
    }
  }

  "jsonp combinator" should {
    "detect jsonp by callback & method parameters" in {
      val handler: HttpRequestHandler[String] = {
        jsonp {
          path("/") {
            get { request: HttpRequest[JValue] =>
              Future(HttpResponse[JValue](content = Some(JString("foo"))))
            }
          }
        }
      }

      val content: String = handler {
        HttpRequest[String](
          method = HttpMethods.GET,
          uri    = "/?callback=jsFunc&method=GET"
        )
      }.value.get.content.get

      content mustEqual """jsFunc("foo",{"headers":{},"status":{"code":200,"reason":""}});"""
    }

    "retrieve POST content from query string parameter" in {
      val handler: HttpRequestHandler[String] = {
        jsonp {
          path("/") {
            post { request: HttpRequest[JValue] =>
              Future(HttpResponse[JValue](content = request.content))
            }
          }
        }
      }

      val content: String = handler {
        HttpRequest[String](
          method  = HttpMethods.GET,
          uri     = "/?callback=jsFunc&method=POST&content=" + encodeUrl("{\"bar\":123}", "UTF-8")
        )
      }.value.get.content.get

      content mustEqual """jsFunc({"bar":123},{"headers":{},"status":{"code":200,"reason":""}});"""
    }

    "retrieve headers from query string parameter" in {
      val handler: HttpRequestHandler[String] = {
        jsonp {
          path("/") {
            get { request: HttpRequest[JValue] =>
              Future(HttpResponse[JValue](content = Some(JString("foo")), headers = request.headers))
            }
          }
        }
      }

      val content: String = handler {
        HttpRequest[String](
          method = HttpMethods.GET,
          uri    = "/?callback=jsFunc&method=GET&headers=" + encodeUrl("{\"bar\":\"123\"}", "UTF-8")
        )
      }.value.get.content.get

      content mustEqual """jsFunc("foo",{"headers":{"bar":"123"},"status":{"code":200,"reason":""}});"""
    }

    "pass undefined to callback when there is no content" in {
      val handler: HttpRequestHandler[String] = {
        jsonp {
          path("/") {
            get { request: HttpRequest[JValue] =>
              Future(HttpResponse[JValue]())
            }
          }
        }
      }

      val content: String = handler {
        HttpRequest[String](
          method = HttpMethods.GET,
          uri    = "/?callback=jsFunc&method=GET&headers=" + encodeUrl("{\"bar\":\"123\"}", "UTF-8")
        )
      }.value.get.content.get

      content mustEqual """jsFunc(undefined,{"headers":{},"status":{"code":200,"reason":""}});"""
    }

    "return headers in 2nd argument to callback function" in {
      val handler: HttpRequestHandler[String] = {
        jsonp {
          path("/") {
            get { request: HttpRequest[JValue] =>
              Future(HttpResponse[JValue](content = Some(JString("foo")), headers = Map("foo" -> "bar")))
            }
          }
        }
      }

      val content: String = handler {
        HttpRequest[String](
          method = HttpMethods.GET,
          uri    = "/?callback=jsFunc&method=GET"
        )
      }.value.get.content.get

      content mustEqual """jsFunc("foo",{"headers":{"foo":"bar"},"status":{"code":200,"reason":""}});"""
    }
  }

  "cookie combinator" should {
    "propagate default cookie value" in {
      val defaultValue = "defaultValue"
      val f = path("/foo/bar") {
        cookie('someCookie ?: defaultValue) { cookieVal =>
          get { (request: HttpRequest[String]) =>
            Future(HttpResponse[String](content=Some(cookieVal)))
          }
        }
      }(HttpRequest[String](HttpMethods.GET, "/foo/bar"))
      f.value must eventually(beSomething)
      f.value.get.content.get must be(defaultValue)
    }
  }

  "parameter combinator" should {
    "extract parameter" in {
      val f = path("/foo/'bar") {
        parameter[String, String]('bar) { bar =>
          get { (request: HttpRequest[String]) =>
            Future(HttpResponse[String](content=Some(bar)))
          }
        }
      }(HttpRequest[String](HttpMethods.GET, "/foo/blahblah"))
      f.value must eventually(beSomething)
      f.value.get.content must beSome("blahblah")
    }

    "put default parameter value into request parameters field when value not specified" in {
      val handler = path("/foo/") {
        parameter[String, String]('bar ?: "bebe") { bar =>
          get { (request: HttpRequest[String]) =>
            request.parameters mustEqual Map('bar -> "bebe")

            Future(HttpResponse[String](content=Some(bar)))
          }
        }
      }

      handler {
        HttpRequest[String](HttpMethods.GET, "/foo/")
      }
    }

    "extract parameter even when combined with produce" in {
      val f = path("/foo/'bar") {
        produce(application/json) {
          parameter[String, JValue]('bar) { bar =>
            get { (request: HttpRequest[String]) =>
              Future(HttpResponse[JValue](content=Some(JString(bar))))
            }
          }
        }
      }(HttpRequest[String](HttpMethods.GET, "/foo/blahblah"))
      f.value must eventually(beSomething)
      f.value.get.content.map(JString(_)) must beSome(JString(""""blahblah""""))
    }
  }

  "path combinator" should {
    "extract symbol" in {
      (path('token) {
        parameter('token) { token =>
          get { (request: HttpRequest[String]) =>
            Future(HttpResponse[String](content=Some(token)))
          }
        }
      }).apply(HttpRequest[String](method = HttpMethods.GET, uri = "A190257C-56F5-499F-A2C6-0FFD0BA7D95B", content = None)).value.get.content must beSome("A190257C-56F5-499F-A2C6-0FFD0BA7D95B")
    }

    "support nested paths" in {
      val f = path("/foo/") {
        path('bar  / "entries") {
          produce(application/json) {
            parameter[String, JValue]('bar) { bar =>
              get { (request: HttpRequest[String]) =>
                Future(HttpResponse[JValue](content=Some(JString(bar))))
              }
            }
          }
        }
      }(HttpRequest[String](HttpMethods.GET, "/foo/blahblah/entries"))
      f.value must eventually(beSomething)
      f.value.get.content.map(JString(_)) must beSome(JString(""""blahblah""""))
    }
  }
}
