package blueeyes.core.service

import org.specs.Specification

import blueeyes.core.http._
import blueeyes.util.Future
import blueeyes.util.FutureImplicits

class HttpRequestHandlerCombinatorsSpec extends Specification{
  "composition of paths" should {
    "have the right type" in {
      /*val handler: HttpRequestHandler[Int] = {
        path("/foo/bar") { 
          path("/baz") {
            get { (request: HttpRequest[Int]) => 
              Future(HttpResponse[Int]())
            }
          }
        }
      }*/
    }
  }
}