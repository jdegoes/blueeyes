package blueeyes.core.service

import org.specs.Specification
import org.specs.matcher.Matchers._

import blueeyes.core.http._
import blueeyes.core.http.MimeType
import blueeyes.core.http.MimeTypes._
import blueeyes.core.data.BijectionsString._
import blueeyes.json.JsonAST._
import blueeyes.util.Future
import blueeyes.util.FutureImplicits

class HttpRequestHandlerCombinatorsSpec extends Specification with HttpRequestHandlerCombinators with RestPathPatternImplicits {
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

  "default cookies should propagate correctly" in {
    val defaultValue = "defaultValue"
    val f = path("/foo/bar") {
      cookie('someCookie, defaultValue) { cookieVal =>
        get { (request: HttpRequest[String]) =>
          Future(HttpResponse[String](content=Some(cookieVal)))
        }
      }
    }(HttpRequest[String](HttpMethods.GET, "/foo/bar"))
    f.value must eventually(beSomething)
    f.value.get.content.get must be(defaultValue)
  }

  "request parameters are available using combinator" in {
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

  "request parameters are available using produce and parameter combinators" in {
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
