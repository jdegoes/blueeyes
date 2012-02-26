package blueeyes.core.service

import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import blueeyes.json.JsonParser

import org.specs2.mutable.Specification

import blueeyes.core.data._
import blueeyes.core.http.HttpStatusCodes.OK
import blueeyes.core.http._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.test.HttpRequestMatchers
import blueeyes.json.JsonAST._
import akka.dispatch.Future
import blueeyes.concurrent.test.FutureMatchers
import blueeyes.util.metrics.DataSize
import DataSize._

import java.net.URLEncoder.{ encode => encodeUrl }
import blueeyes.core.data.{ Chunk, ByteChunk, Bijection, GZIPByteChunk }
import scalaz.Success

class HttpRequestHandlerCombinatorsSpec extends Specification with HttpRequestHandlerCombinators with RestPathPatternImplicits with HttpRequestHandlerImplicits 
with blueeyes.bkka.AkkaDefaults with HttpRequestMatchers {

  import BijectionsChunkFutureJson._
  import BijectionsChunkString._

  "composition of paths" should {
    "have the right type" in {
      val handler: AsyncHttpService[Int] = {
        path("/foo/bar") {
          path("/baz") {
            get { (request: HttpRequest[Int]) =>
              Future(HttpResponse[Int]())
            }
          }
        }
      }

      handler must_== handler
    }
  }

  "jsonp combinator" should {
    "detect jsonp by callback & method parameters" in {
      val handler: AsyncHttpService[ByteChunk] = {
        jsonp[ByteChunk] {
          path("/") {
            get { request: HttpRequest[Future[JValue]] =>
              Future(HttpResponse[JValue](content = Some(JString("foo"))))
            }
          }
        }
      }

      handler.service(HttpRequest[ByteChunk](method = HttpMethods.GET, uri = "/?callback=jsFunc&method=GET")).
      toOption.get.map(_.content.map(ChunkToString)) must whenDelivered {
        beSome("""jsFunc("foo",{"headers":{},"status":{"code":200,"reason":""}});""")
      }
    }

    "retrieve POST content from query string parameter" in {
      val handler: AsyncHttpService[ByteChunk] = {
        jsonp {
          path("/") {
            post { request: HttpRequest[Future[JValue]] =>
              request.content match {
                case Some(future) => future.map(c => HttpResponse[JValue](content = Some(c)))
                case None => Future(HttpResponse[JValue](content = None))
              }
            }
          }
        }
      }

      val request = HttpRequest[ByteChunk](method = HttpMethods.GET,
                                           uri = "/?callback=jsFunc&method=POST&content=" + encodeUrl("{\"bar\":123}", "UTF-8"))

      handler.service(request).map(_.map(_.content.map(ChunkToString))) must beLike {
        case Success(future) => future must whenDelivered {
          beSome {
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
              Future(HttpResponse[JValue](content = Some(JString("foo")), headers = request.headers))
            }
          }
        }
      }

      val request = HttpRequest[ByteChunk](method = HttpMethods.GET,
                                           uri = "/?callback=jsFunc&method=GET&headers=" + encodeUrl("{\"bar\":\"123\"}", "UTF-8"))

      handler.service(request).map(_.map(_.content.map(ChunkToString))) must {
        beLike {
          case Success(future) => future must whenDelivered {
            beSome("""jsFunc("foo",{"headers":{"bar":"123"},"status":{"code":200,"reason":""}});""")
          }
        }
      }
    }

    "pass undefined to callback when there is no content" in {
      val handler: AsyncHttpService[ByteChunk] = {
        jsonp {
          path("/") {
            get { request: HttpRequest[Future[JValue]] =>
              Future(HttpResponse[JValue]())
            }
          }
        }
      }

      val request = HttpRequest[ByteChunk](method = HttpMethods.GET,
                                           uri = "/?callback=jsFunc&method=GET&headers=" + encodeUrl("{\"bar\":\"123\"}", "UTF-8"))

      handler.service(request).map(_.map(_.content.map(ChunkToString))) must beLike {
        case Success(future) => future must whenDelivered {
          beSome("""jsFunc(undefined,{"headers":{},"status":{"code":200,"reason":""}});""")
        }
      }
    }

    "return headers in 2nd argument to callback function" in {
      val handler: AsyncHttpService[ByteChunk] = {
        jsonp {
          path("/") {
            get { request: HttpRequest[Future[JValue]] =>
              Future(HttpResponse[JValue](content = Some(JString("foo")), headers = Map("foo" -> "bar")))
            }
          }
        }
      }

      val request = HttpRequest[ByteChunk]( method = HttpMethods.GET, uri = "/?callback=jsFunc&method=GET")

      handler.service(request).map(_.map(_.content.map(ChunkToString))) must beLike {
        case Success(future) => future must whenDelivered {
          beSome {
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
              Future(HttpResponse[JValue](status = HttpStatus(400, "Funky request."), content = Some(JString("bang"))))
            }
          }
        }
      }

      val request = HttpRequest[ByteChunk](method = HttpMethods.GET, uri = "/?callback=jsFunc&method=GET") 
      
      errorHandler.service(request) must beLike {
        case Success(future) => future must succeedWithContent {
          (byteChunk: ByteChunk) => ChunkToString(byteChunk) must_== """jsFunc("bang",{"headers":{},"status":{"code":400,"reason":"Funky request."}});"""
        }
      }
    }
  }

  "cookie combinator" should {
    "propagate default cookie value" in {
      val defaultValue = "defaultValue"
      val handler = path("/foo/bar") {
        cookie('someCookie, Some(defaultValue)) {
          get { (request: HttpRequest[String]) =>
            {
              cookieVal: String => Future(HttpResponse[String](content = Some(cookieVal)))
            }
          }
        }
      }
      
      handler.service(HttpRequest[String](HttpMethods.GET, "/foo/bar")) must beLike {
        case Success(future) => future must succeedWithContent(be_==(defaultValue))
      }
    }
  }

  "parameter combinator" should {
    "extract parameter" in {
      val handler = path("/foo/'bar") {
        parameter('bar) {
          get { (request: HttpRequest[String]) =>
            (bar: String) =>
              Future(HttpResponse[String](content = Some(bar)))
          }
        }
      }
      
      handler.service(HttpRequest[String](HttpMethods.GET, "/foo/blahblah")) must beLike {
        case Success(future) => future must succeedWithContent(be_==("blahblah"))
      }
    }

    "put default parameter value into request parameters field when value not specified" in {
      val handler = path("/foo/") {
        parameter[String, Future[HttpResponse[String]]]('bar, Some("bebe")) {
          get { (request: HttpRequest[String]) =>
            (bar: String) => {
              request.parameters mustEqual Map('bar -> "bebe")
              Future(HttpResponse[String](content = request.parameters.get('bar)))
            }
          }
        }
      }

      handler.service(HttpRequest[String](HttpMethods.GET, "/foo/")) must beLike {
        case Success(future) => future must succeedWithContent(be_==("bebe"))
      }
    }

    "extract parameter even when combined with produce" in {
      val handler = path("/foo/'bar") {
        produce(application / json) {
          parameter('bar) {
            get { (request: HttpRequest[String]) =>
              (bar: String) =>
                Future(HttpResponse[JValue](content = Some(JString(bar))))
            }
          }
        }
      }
      
      handler.service(HttpRequest[String](HttpMethods.GET, "/foo/blahblah")) must beLike {
        case Success(future) => future must succeedWithContent(be_==(JString("blahblah")))
      }
    }

    "extract decoded parameter" in {
      val handler = path("/foo/'bar") {
        produce(application / json) {
          parameter('bar) {
            get { (request: HttpRequest[String]) =>
              bar: String =>
                Future(HttpResponse[JValue](content = Some(JString(bar))))
            }
          }
        }
      }
      
      handler.service(HttpRequest[String](HttpMethods.GET, "/foo/blah%20blah")) must beLike {
        case Success(future) => future must succeedWithContent(be_==(JString("blah blah")))
      }
    }
  }

  "path combinator" should {
    "extract symbol" in {
      val handler = path('token) {
        parameter('token) {
          get {
            service((request: HttpRequest[String]) => { (token: String) => Future(HttpResponse[String](content = Some(token))) })
          }
        }
      }

      handler.service(HttpRequest[String](method = HttpMethods.GET, uri = "A190257C-56F5-499F-A2C6-0FFD0BA7D95B")) must beLike {
        case Success(future) => future must succeedWithContent(be_==("A190257C-56F5-499F-A2C6-0FFD0BA7D95B"))
      }
    }

    "support nested paths" in {
      val handler = path("/foo/") {
        path('bar / "entries") {
          produce(application / json) {
            parameter('bar) {
              get { (request: HttpRequest[String]) =>
                bar: String =>
                  Future(HttpResponse[JValue](content = Some(JString(bar))))
              }
            }
          }
        }
      }
      
      handler.service(HttpRequest[String](HttpMethods.GET, "/foo/blahblah/entries")) must beLike {
        case Success(future) => future must succeedWithContent(be_==(JString("blahblah")))
      }
    }
  }

  "compress combinator" should {
    "compress content if request contains accept encoding header" in {
      val chunk = Chunk(Array[Byte]('1', '2'))
      val handler = compress {
        path("/foo") {
          get { (request: HttpRequest[ByteChunk]) =>
            Future(HttpResponse[ByteChunk](content = request.content))
          }
        }
      }

      handler.service(HttpRequest[ByteChunk](method = HttpMethods.GET, uri = "/foo", content = Some(chunk), headers = HttpHeaders.Empty + `Accept-Encoding`(Encodings.gzip, Encodings.compress))) must beLike {
        case Success(future) => future must succeedWithContent {
          (v: ByteChunk) => new String(v.data) must_== new String(GZIPByteChunk(chunk).data)
        }
      }
    }

    "does not compress content if request does not contain accept appropriate encoding header" in {
      val chunk = Chunk(Array[Byte]('1', '2'))
      val handler =compress {
        path("/foo") {
          get { (request: HttpRequest[ByteChunk]) =>
            Future(HttpResponse[ByteChunk](content = request.content))
          }
        }
      }
      
      handler.service(HttpRequest[ByteChunk](method = HttpMethods.GET, uri = "/foo", content = Some(chunk), headers = HttpHeaders.Empty + `Accept-Encoding`(Encodings.compress))) must beLike {
        case Success(future) => future must succeedWithContent {
          (v: ByteChunk) => new String(v.data) must_== "12"
        }
      }
    }
  }

  "aggregate combinator" should {
    "aggregate full content when size is not specified" in {
      val handler = aggregate(None) {
        path("/foo") {
          get { (request: HttpRequest[Future[ByteChunk]]) =>
            request.content.map {
              _.flatMap { content =>
                Future(HttpResponse[ByteChunk](content = Some(content)))
              }
            }.getOrElse(Future(HttpResponse[ByteChunk]()))
          }
        }
      }
      
      handler.service(HttpRequest[ByteChunk](method = HttpMethods.GET, uri = "/foo", content = Some(Chunk(Array[Byte]('1', '2'), Some(Future(Chunk(Array[Byte]('3', '4')))))))) must beLike {
        case Success(future) => future must succeedWithContent {
          (v: ByteChunk) => new String(v.data) must_== "1234"
        }
      }
    }

    "aggregate content up to the specified size" in {
      val handler = aggregate(Some(2.bytes)) {
        path("/foo") {
          get { (request: HttpRequest[Future[ByteChunk]]) =>
            request.content.map {
              _.flatMap { content =>
                Future(HttpResponse[ByteChunk](content = Some(content)))
              }
            }.getOrElse(Future(HttpResponse[ByteChunk]()))
          }
        }
      }
      
      handler.service(HttpRequest[ByteChunk](method = HttpMethods.GET, uri = "/foo", content = Some(Chunk(Array[Byte]('1', '2'), Some(Future(Chunk(Array[Byte]('3', '4')))))))) must beLike {
        case Success(future) => future must succeedWithContent {
          (v: ByteChunk) => new String(v.data) must_== "12"
        }
      }
    }
  }

  "decodeUrl combinator" should {
    "decode request URI" in {
      val svc = path("/foo/'bar") {
        produce(application / json) {
          decodeUrl {
            get { (request: HttpRequest[String]) =>
              Future(HttpResponse[JValue](content = Some(JString(request.uri.toString))))
            }
          }
        }
      }

      svc.service(HttpRequest[String](HttpMethods.GET, "/foo/blah%20blah")) must beLike {
        case Success(future) => future must succeedWithContent(be_==(JString("/foo/blah blah")))
      }
    }
  }
}
