package blueeyes.core.service

import org.specs.Specification
import org.specs.matcher.Matchers._

import blueeyes.core.http._
import blueeyes.util.Future
import blueeyes.util.FutureImplicits

class HttpRequestHandlerCombinatorsSpec extends Specification with HttpRequestHandlerCombinators with RestPathPatternImplicits{
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
    }
  }

  "default cookies should propagate correctly" in {
    val defaultValue = "defaultValue"
    val f = path("/foo/bar") { 
      cookie('someCookie, Some(defaultValue)) { cookieVal =>
        get { (request: HttpRequest[String]) => 
          Future(HttpResponse[String](content=Some(cookieVal)))
        }
      }
    }(HttpRequest[String](HttpMethods.GET, "/foo/bar"))
    f.value must eventually(beSomething)
    f.value.get.content.get must be(defaultValue)
  }
}
