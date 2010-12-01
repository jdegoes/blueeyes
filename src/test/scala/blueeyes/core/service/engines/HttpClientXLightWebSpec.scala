package blueeyes.core.service.engines

import org.specs.Specification
import org.specs.util._
import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpHeaderImplicits
import blueeyes.core.http.MimeTypes._
import blueeyes.core.service._
import blueeyes.util.Future
import blueeyes.util.FutureImplicits

class HttpClientXLightWebSpec extends Specification with HttpResponseHandlerCombinators with FutureImplicits {
  import HttpHeaderImplicits._

  val duration = 250
  val retries = 10
  val skip = true
  
  def skipper(): () => Unit = skip match {
    case true => skip("Will use Skalatra")
    case _ => () => Unit
  }

  private val httpClient = new HttpClientXLightWebEnginesString { }

  "Support GET requests with status OK" in {
    skipper()()
    val f = protocol("http") {
      host("localhost") {
        path[String, HttpResponse[String]]("/test/echo.php") {
          get[String, HttpResponse[String]] { r => r }
        }
      }
    }(httpClient)
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.status.code must eventually(be(HttpStatusCodes.OK))
  }
 
 "Support GET requests with status Not Found" in {
   skipper()()
   val f = protocol("http") {
     host("localhost") {
       path[String, HttpResponse[String]]("/test/bogus") {
         get[String, HttpResponse[String]]{ r => r}
       }
     }
   }(httpClient)
   f.value must eventually(retries, new Duration(duration))(beSomething)
   f.value.get.status.code must be(HttpStatusCodes.NotFound)
  }

  "Support GET requests with query params" in {
    skipper()()
    val f = protocol("http") {
      host("localhost") {
        path[String, HttpResponse[String]]("/test/echo.php?param1=a&param2=b") {
          get[String, HttpResponse[String]] { (res: HttpResponse[String]) =>
	    res
          }
        }
      }
    }(httpClient)

    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with query params" in {
    skipper()()
    val f = protocol("http") {
      host("localhost") {
        path[String, HttpResponse[String]]("/test/echo.php?param1=a&param2=b") {
          post[String, HttpResponse[String]]("") { r => r }
        }
      }
    }(httpClient)
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with request params" in {
    skipper()()
    val f = protocol("http") {
      host("localhost") {
        path[String, HttpResponse[String]]("/test/echo.php") {
          parameters('param1 -> "a", 'param2 -> "b") {
            post[String, HttpResponse[String]]("") { r => r }
          }
        }
      }
    }(httpClient)
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with body" in {
    skipper()()
    val content = "Hello, world"
    val f = protocol("http") {
      host("localhost") {
        path[String, HttpResponse[String]]("/test/echo.php") {
          post[String, HttpResponse[String]](content) { r => r }
        }
      }
    }(httpClient)
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must eventually(equalIgnoreSpace(content))
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with body and request params" in {
    skipper()()
    val content = "Hello, world"
    val f = protocol("http") {
      host("localhost") {
        path[String, HttpResponse[String]]("/test/echo.php") {
          parameters('param1 -> "a", 'param2 -> "b") {
            post[String, HttpResponse[String]](content) { r => r }
          }
        }
      }
    }(httpClient)
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must equalIgnoreSpace("param1=a&param2=b" + content)
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support PUT requests with body" in {
    skipper()()
    val content = "Hello, world"
    val f = protocol("http") {
      host("localhost") {
        path[String, HttpResponse[String]]("/test/echo.php") {
          headers(`Content-Length`(100)) {
            put[String, HttpResponse[String]](content) { r => r }
          }
        }
      }
    }(httpClient)
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support GET requests with header" in {
    skipper()()
    val f = protocol("http") {
      host("localhost") {
        path[String, HttpResponse[String]]("/test/echo.php?headers=true") {
          headers("Fooblahblah" -> "washere", "param2" -> "1") {
            get[String, HttpResponse[String]] { r => r }
          }
        }
      }
    }(httpClient)
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must include("Fooblahblah: washere")
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with Content-Type: text/html & Content-Length: 100" in {
    skipper()()
    val content = "<html></html>"
    val f = protocol("http") {
      host("localhost") {
        path[String, HttpResponse[String]]("/test/echo.php") {
          headers(`Content-Type`(text/html), `Content-Length`(100)) {
            post[String, HttpResponse[String]](content) { r => r }
          }
        }
      }
    }(httpClient)
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must beEqual(content)
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with large payload" in {
    skipper()()
    val content = Array.fill(1024*1000)(0).toList.mkString("")
    val f = protocol("http") {
      host("localhost") {
        path[String, HttpResponse[String]]("/test/echo.php") {
          post[String, HttpResponse[String]](content) { r => r }
        }
      }
    }(httpClient)
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must beEqual(content)
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support HEAD requests" in {
    skipper()()
    val f = protocol("http") {
      host("localhost") {
        path[String, HttpResponse[String]]("/test/echo.php") {
          head[String, HttpResponse[String]]() { r => r }
        }
      }
    }(httpClient)
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support CONNECT requests" in {
    skip("CONNECT method TBD")
  }

  "Support GET requests of 1000 requests" in {
    skipper()()
    val total = 1000
    val duration = 1000
    val futures = (0 until total).map { i =>
      protocol("http") {
        host("localhost") {
          path[String, HttpResponse[String]]("/test/echo.php?test=true") {
              get[String, HttpResponse[String]] { r => r }
          }
        }
      }(httpClient)
    }

    val responses = futures.foldLeft(0) { 
      (acc, f) => {
        f.value must eventually(retries, new Duration(duration))(beSomething)
        f.value.get.status.code must be(HttpStatusCodes.OK)
        acc + 1
      }
    }

    responses must beEqual(total)
  }
}
