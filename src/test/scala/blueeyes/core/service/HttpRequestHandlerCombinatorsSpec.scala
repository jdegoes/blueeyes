package blueeyes.core.service

import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import blueeyes.json.JsonParser

import org.specs.Specification
import org.specs.matcher.Matchers._

import blueeyes.core.data._
import blueeyes.core.http.HttpStatusCodes.OK
import blueeyes.core.http._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.HttpHeaders._
import blueeyes.json.JsonAST._
import blueeyes.concurrent.Future
import blueeyes.concurrent.test.FutureMatchers
import blueeyes.util.metrics.DataSize
import DataSize._

import java.net.URLEncoder.{ encode => encodeUrl }
import blueeyes.core.data.{ ByteMemoryChunk, ByteChunk, Bijection, GZIPByteChunk }
import scalaz.Success

class HttpRequestHandlerCombinatorsSpec extends Specification with HttpRequestHandlerCombinators with RestPathPatternImplicits with HttpRequestHandlerImplicits with FutureMatchers {
  import BijectionsChunkFutureJson._
  import BijectionsChunkString._

  "composition of paths" should {
    "have the right type" in {
      val handler: AsyncHttpService[Int] = {
        path("/foo/bar") {
          path("/baz") {
            get { (request: HttpRequest[Int]) =>
              Future.sync(HttpResponse[Int]())
            }
          }
        }
      }

      handler mustBe handler
    }
  }

  "jsonp combinator" should {
    "detect jsonp by callback & method parameters" in {
      val handler: AsyncHttpService[ByteChunk] = {
        jsonp[ByteChunk] {
          path("/") {
            get { request: HttpRequest[Future[JValue]] =>
              Future.sync(HttpResponse[JValue](content = Some(JString("foo"))))
            }
          }
        }
      }

      handler.service {
        HttpRequest[ByteChunk](
          method = HttpMethods.GET,
          uri = "/?callback=jsFunc&method=GET")
      }.toOption.get must whenDelivered {
        verify {
          _.content.map(ChunkToString) must beSome(
            """jsFunc("foo",{"headers":{},"status":{"code":200,"reason":""}});""")
        }
      }
    }

    "retrieve POST content from query string parameter" in {
      val handler: AsyncHttpService[ByteChunk] = {
        jsonp {
          path("/") {
            post { request: HttpRequest[Future[JValue]] =>
              Future.sync(HttpResponse[JValue](content = request.content.flatMap(_.value)))
            }
          }
        }
      }

      handler.service {
        HttpRequest[ByteChunk](
          method = HttpMethods.GET,
          uri = "/?callback=jsFunc&method=POST&content=" + encodeUrl("{\"bar\":123}", "UTF-8"))
      }.toOption.get must whenDelivered {
        verify {
          _.content.map(ChunkToString) must beSome {
            """jsFunc({"bar":123},{"headers":{},"status":{"code":200,"reason":""}});"""
          }
        }
      }
    }

    "retrieve headers from query string parameter" in {
      val handler: AsyncHttpService[ByteChunk] = {
        jsonp {
          path("/") {
            get { request: HttpRequest[Future[JValue]] =>
              Future.sync(HttpResponse[JValue](content = Some(JString("foo")), headers = request.headers))
            }
          }
        }
      }

      handler.service {
        HttpRequest[ByteChunk](
          method = HttpMethods.GET,
          uri = "/?callback=jsFunc&method=GET&headers=" + encodeUrl("{\"bar\":\"123\"}", "UTF-8"))
      }.toOption.get must whenDelivered {
        verify {
          _.content.map(ChunkToString) must beSome {
            """jsFunc("foo",{"headers":{"bar":"123"},"status":{"code":200,"reason":""}});"""
          }
        }
      }
    }

    "pass undefined to callback when there is no content" in {
      val handler: AsyncHttpService[ByteChunk] = {
        jsonp {
          path("/") {
            get { request: HttpRequest[Future[JValue]] =>
              Future.sync(HttpResponse[JValue]())
            }
          }
        }
      }

      handler.service {
        HttpRequest[ByteChunk](
          method = HttpMethods.GET,
          uri = "/?callback=jsFunc&method=GET&headers=" + encodeUrl("{\"bar\":\"123\"}", "UTF-8"))
      }.toOption.get must whenDelivered {
        verify {
          _.content.map(ChunkToString) must beSome {
            """jsFunc(undefined,{"headers":{},"status":{"code":200,"reason":""}});"""
          }
        }
      }
    }

    "return headers in 2nd argument to callback function" in {
      val handler: AsyncHttpService[ByteChunk] = {
        jsonp {
          path("/") {
            get { request: HttpRequest[Future[JValue]] =>
              Future.sync(HttpResponse[JValue](content = Some(JString("foo")), headers = Map("foo" -> "bar")))
            }
          }
        }
      }

      handler.service {
        HttpRequest[ByteChunk](
          method = HttpMethods.GET,
          uri = "/?callback=jsFunc&method=GET")
      }.toOption.get must whenDelivered {
        verify {
          _.content.map(ChunkToString) must beSome {
            """jsFunc("foo",{"headers":{"foo":"bar"},"status":{"code":200,"reason":""}});"""
          }
        }
      }
    }


    "return 200 and propigate status to callback under failure scenarios" in {
      val errorHandler: AsyncHttpService[ByteChunk] = {
        jsonp[ByteChunk] {
          path("/") {
            get { request: HttpRequest[Future[JValue]] =>
              Future.sync(HttpResponse[JValue](status = HttpStatus(400, "Funky request."), content = Some(JString("bang"))))
            }
          }
        }
      }
      errorHandler.service {
        HttpRequest[ByteChunk](
          method = HttpMethods.GET,
          uri = "/?callback=jsFunc&method=GET")
      }.toOption.get must whenDelivered {
        verify { resp: HttpResponse[ByteChunk] =>
          resp.status.code mustBe OK
          resp.content.map(ChunkToString) must beSome {
            """jsFunc("bang",{"headers":{},"status":{"code":400,"reason":"Funky request."}});"""
          }
        }
      }     
    }
  }

  "cookie combinator" should {
    "propagate default cookie value" in {
      val defaultValue = "defaultValue"
      val f = path("/foo/bar") {
        cookie('someCookie, Some(defaultValue)) {
          get { (request: HttpRequest[String]) =>
            {
              cookieVal: String => Future.sync(HttpResponse[String](content = Some(cookieVal)))
            }
          }
        }
      }.service(HttpRequest[String](HttpMethods.GET, "/foo/bar"))
      f.toOption.get.value must eventually(beSomething)
      f.toOption.get.value.get.content.get must be(defaultValue)
    }
  }

  "parameter combinator" should {
    "extract parameter" in {
      val f = path("/foo/'bar") {
        parameter('bar) {
          get { (request: HttpRequest[String]) =>
            (bar: String) =>
              Future.sync(HttpResponse[String](content = Some(bar)))
          }
        }
      }.service(HttpRequest[String](HttpMethods.GET, "/foo/blahblah"))
      f.toOption.get.value must eventually(beSomething)
      f.toOption.get.value.get.content must beSome("blahblah")
    }

    "put default parameter value into request parameters field when value not specified" in {
      val handler = path("/foo/") {
        parameter[String, Future[HttpResponse[String]]]('bar, Some("bebe")) {
          get { (request: HttpRequest[String]) =>
            (bar: String) =>
              request.parameters mustEqual Map('bar -> "bebe")

              Future.sync(HttpResponse[String](content = Some(bar)))
          }
        }
      }

      handler.service {
        HttpRequest[String](HttpMethods.GET, "/foo/")
      }
    }

    "extract parameter even when combined with produce" in {
      val f = path("/foo/'bar") {
        produce(application / json) {
          parameter('bar) {
            get { (request: HttpRequest[String]) =>
              (bar: String) =>
                Future.sync(HttpResponse[JValue](content = Some(JString(bar))))
            }
          }
        }
      }.service(HttpRequest[String](HttpMethods.GET, "/foo/blahblah"))
      f.toOption.get.value must eventually(beSomething)
      f.toOption.get.value.get.content must beSome(JString("blahblah"))
    }

    "extract decoded parameter" in {
      val f = path("/foo/'bar") {
        produce(application / json) {
          parameter('bar) {
            get { (request: HttpRequest[String]) =>
              bar: String =>
                Future.sync(HttpResponse[JValue](content = Some(JString(bar))))
            }
          }
        }
      }.service(HttpRequest[String](HttpMethods.GET, "/foo/blah%20blah"))
      f.toOption.get.value must eventually(beSomething)
      f.toOption.get.value.get.content must beSome(JString("blah blah"))
    }
  }

  "path combinator" should {
    "extract symbol" in {
      val s = path('token) {
        parameter('token) {
          get {
            service((request: HttpRequest[String]) => { (token: String) => Future.sync(HttpResponse[String](content = Some(token))) })
          }
        }
      }

      s.service(HttpRequest[String](method = HttpMethods.GET, uri = "A190257C-56F5-499F-A2C6-0FFD0BA7D95B"))
        .toOption.get.value.get.content must beSome("A190257C-56F5-499F-A2C6-0FFD0BA7D95B")
    }

    "support nested paths" in {
      val f = path("/foo/") {
        path('bar / "entries") {
          produce(application / json) {
            parameter('bar) {
              get { (request: HttpRequest[String]) =>
                bar: String =>
                  Future.sync(HttpResponse[JValue](content = Some(JString(bar))))
              }
            }
          }
        }
      }.service(HttpRequest[String](HttpMethods.GET, "/foo/blahblah/entries"))
      f.toOption.get.value must eventually(beSomething)
      f.toOption.get.value.get.content must beSome(JString("blahblah"))
    }
  }

  "compress combinator" should {
    "compress content if request contains accept encoding header" in {
      val chunk = new ByteMemoryChunk(Array[Byte]('1', '2'), () => None)
      val handler = compress {
        path("/foo") {
          get { (request: HttpRequest[ByteChunk]) =>
            Future.sync(HttpResponse[ByteChunk](content = request.content))
          }
        }
      }
      val response = handler.service(HttpRequest[ByteChunk](method = HttpMethods.GET, uri = "/foo", content = Some(chunk), headers = HttpHeaders.Empty + `Accept-Encoding`(Encodings.gzip, Encodings.compress)))
      response.toOption.get.value.get.content.map(v => new String(v.data)) must beSome(new String(GZIPByteChunk(chunk).data))
    }
    "does not compress content if request does not contain accept appropriate encoding header" in {
      val chunk = new ByteMemoryChunk(Array[Byte]('1', '2'), () => None)
      (compress {
        path("/foo") {
          get { (request: HttpRequest[ByteChunk]) =>
            Future.sync(HttpResponse[ByteChunk](content = request.content))
          }
        }
      }).service(HttpRequest[ByteChunk](method = HttpMethods.GET, uri = "/foo", content = Some(chunk), headers = HttpHeaders.Empty + `Accept-Encoding`(Encodings.compress))).toOption.get.value.get.content.map(v => new String(v.data)) must beSome("12")
    }
  }
  "aggregate combinator" should {
    "aggregate full content when size is not specified" in {
      (aggregate(None) {
        path("/foo") {
          get { (request: HttpRequest[Future[ByteChunk]]) =>
            request.content.map {
              _.flatMap { content =>
                Future.sync(HttpResponse[ByteChunk](content = Some(content)))
              }
            }.getOrElse(Future.sync(HttpResponse[ByteChunk]()))
          }
        }
      }).service(HttpRequest[ByteChunk](method = HttpMethods.GET, uri = "/foo", content = Some(new ByteMemoryChunk(Array[Byte]('1', '2'), () => Some(Future.sync(new ByteMemoryChunk(Array[Byte]('3', '4')))))))).toOption.get.value.get.content.map(v => new String(v.data)) must beSome("1234")
    }
    "aggregate content up to the specified size" in {
      (aggregate(Some(2.bytes)) {
        path("/foo") {
          get { (request: HttpRequest[Future[ByteChunk]]) =>
            request.content.map {
              _.flatMap { content =>
                Future.sync(HttpResponse[ByteChunk](content = Some(content)))
              }
            }.getOrElse(Future.sync(HttpResponse[ByteChunk]()))
          }
        }
      }).service(HttpRequest[ByteChunk](method = HttpMethods.GET, uri = "/foo", content = Some(new ByteMemoryChunk(Array[Byte]('1', '2'), () => Some(Future.sync(new ByteMemoryChunk(Array[Byte]('3', '4')))))))).toOption.get.value.get.content.map(v => new String(v.data)) must beSome("12")
    }
  }

  "decodeUrl combinator" should {
    "decode request URI" in {
      val svc = path("/foo/'bar") {
        produce(application / json) {
          decodeUrl {
            get { (request: HttpRequest[String]) =>
              Future.sync(HttpResponse[JValue](content = Some(JString(request.uri.toString))))
            }
          }
        }
      }

      svc.service(HttpRequest[String](HttpMethods.GET, "/foo/blah%20blah")) must beLike {
        case Success(future) => future must whenDelivered {
          verify {
            _.content must beSome(JString("/foo/blah blah"))
          }
        }
      }
    }
  }
}
