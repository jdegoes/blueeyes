package blueeyes.core.service


import akka.dispatch.Future
import akka.dispatch.Await
import akka.util._

import blueeyes.bkka._
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpStatusCodes.OK
import blueeyes.core.http.MimeTypes._
import blueeyes.json._
import blueeyes.json.JParser
import blueeyes.util.metrics.DataSize
import DataSize._
import ByteChunk._

import java.net.URLEncoder.{ encode => encodeUrl }
import java.nio.ByteBuffer

import org.specs2.mutable.Specification
import blueeyes.core.http.test.HttpRequestMatchers
import blueeyes.akka_testing.FutureMatchers

import scalaz._
import scalaz.syntax.monad._

class HttpRequestHandlerCombinatorsSpec extends Specification
    with HttpRequestHandlerCombinators
    with RestPathPatternImplicits
    with HttpRequestHandlerImplicits
    with TestAkkaDefaults
    with HttpRequestMatchers {

  import DefaultBijections._
  sequential

  def stream = ByteBuffer.wrap(Array[Byte]('1', '2')) :: Future(ByteBuffer.wrap(Array[Byte]('3', '4'))).liftM[StreamT]

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
        jsonp { transcode {
          path("/") {
            get { (request: HttpRequest[Future[JValue]]) =>
              Future(HttpResponse[JValue](content = Some(JString("foo"))))
            }
          }
        }}
      }

      handler.service(HttpRequest[ByteChunk](method = HttpMethods.GET, uri = "/?callback=jsFunc&method=GET")) must beLike {
        case Success(future) =>
          future.flatMap(response => futureStringToChunk.unapply(response.content.get)) must whenDelivered {
            be_==("""jsFunc("foo",{"headers":{},"status":{"code":200,"reason":""}});""")
          }
      }
    }

    "retrieve POST content from query string parameter" in {
      val handler: AsyncHttpService[ByteChunk] = {
        jsonp { transcode {
          path("/") {
            post {
              (_: HttpRequest[Future[JValue]]).content match {
                case Some(future) => future.map(c => HttpResponse[JValue](content = Some(c)))
                case None => Future(HttpResponse[JValue](content = None))
              }
            }
          }
        }}
      }

      val request = HttpRequest[ByteChunk](method = HttpMethods.GET,
                                           uri = "/?callback=jsFunc&method=POST&content=" + encodeUrl("{\"bar\":123}", "UTF-8"))

      handler.service(request) must beLike {
        case Success(future) =>
          future.flatMap(response => futureStringToChunk.unapply(response.content.get)) must whenDelivered {
            be_==("""jsFunc({"bar":123},{"headers":{},"status":{"code":200,"reason":""}});""")
          }
      }
    }

    "retrieve headers from query string parameter" in {
      val handler: AsyncHttpService[ByteChunk] = {
        jsonp { transcode {
          path("/") {
            get { (request: HttpRequest[Future[JValue]]) =>
              Future(HttpResponse[JValue](content = Some(JString("foo")), headers = request.headers))
            }
          }
        }}
      }

      val request = HttpRequest[ByteChunk](method = HttpMethods.GET,
                                           uri = "/?callback=jsFunc&method=GET&headers=" + encodeUrl("{\"bar\":\"123\"}", "UTF-8"))

      handler.service(request) must beLike {
        case Success(future) =>
          future.flatMap(response => futureStringToChunk.unapply(response.content.get)) must whenDelivered {
            be_==("""jsFunc("foo",{"headers":{"bar":"123"},"status":{"code":200,"reason":""}});""")
          }
      }
    }

    "pass undefined to callback when there is no content" in {
      val handler: AsyncHttpService[ByteChunk] = {
        jsonp { transcode {
          path("/") {
            get { (request: HttpRequest[Future[JValue]]) =>
              Future(HttpResponse[JValue]())
            }
          }
        }}
      }

      val request = HttpRequest[ByteChunk](method = HttpMethods.GET,
                                           uri = "/?callback=jsFunc&method=GET&headers=" + encodeUrl("{\"bar\":\"123\"}", "UTF-8"))

      handler.service(request) must beLike {
        case Success(future) =>
          future.flatMap(response => futureStringToChunk.unapply(response.content.get)) must whenDelivered {
            be_==("""jsFunc(undefined,{"headers":{},"status":{"code":200,"reason":""}});""")
          }
      }
    }

    "return headers in 2nd argument to callback function" in {
      val handler: AsyncHttpService[ByteChunk] = {
        jsonp { transcode[ByteChunk, JValue] {
          path("/") {
            get { (request: HttpRequest[Future[JValue]]) =>
              Future(HttpResponse[JValue](content = Some(JString("foo")), headers = Map("foo" -> "bar")))
            }
          }
        }}
      }

      val request = HttpRequest[ByteChunk]( method = HttpMethods.GET, uri = "/?callback=jsFunc&method=GET")

      handler.service(request) must beLike {
        case Success(future) =>
          future.flatMap(response => futureStringToChunk.unapply(response.content.get)) must whenDelivered {
            be_==("""jsFunc("foo",{"headers":{"foo":"bar"},"status":{"code":200,"reason":""}});""")
          }
      }
    }


    "return 200 and propigate status to callback under failure scenarios" in {
      val errorHandler: AsyncHttpService[ByteChunk] = {
        jsonp { transcode {
          path("/") {
            get { (request: HttpRequest[Future[JValue]]) =>
              Future(HttpResponse[JValue](status = HttpStatus(400, "Funky request."), content = Some(JString("bang"))))
            }
          }
        }}
      }

      val request = HttpRequest[ByteChunk](method = HttpMethods.GET, uri = "/?callback=jsFunc&method=GET")

      errorHandler.service(request) must beLike {
        case Success(future) =>
          future must succeedWithContent { (byteChunk: ByteChunk) =>
            futureStringToChunk.unapply(byteChunk) must whenDelivered {
              be_==("""jsFunc("bang",{"headers":{},"status":{"code":400,"reason":"Funky request."}});""")
            }
          }
      }
    }
  }

  "cookie combinator" should {
    "propagate default cookie value" in {
      val defaultValue = "defaultValue"
      val handler = path("/foo/bar") {
        cookie('someCookie, Some(defaultValue)) {
          get {
            service {
              (request: HttpRequest[String]) => {
                cookieVal: String => Future(HttpResponse[String](content = Some(cookieVal)))
              }
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
          get {
            service {
              (request: HttpRequest[String]) => (bar: String) => Future(HttpResponse[String](content = Some(bar)))
            }
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
          get {
            service {
              (request: HttpRequest[String]) => (bar: String) => {
                request.parameters mustEqual Map('bar -> "bebe")
                Future(HttpResponse[String](content = request.parameters.get('bar)))
              }
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
        produce[String, JValue, JValue](application / json) {
          parameter('bar) {
            get {
              service {
                (request: HttpRequest[String]) => (bar: String) => Future(HttpResponse[JValue](content = Some(JString(bar))))
              }
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
        produce[String, JValue, JValue](application / json) {
          parameter('bar) {
            get {
              service {
                (request: HttpRequest[String]) => (bar: String) => Future(HttpResponse[JValue](content = Some(JString(bar))))
              }
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
            service {
              (request: HttpRequest[String]) => {
                (token: String) => Future(HttpResponse[String](content = Some(token)))
              }
            }
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
          produce[String, JValue, JValue](application / json) {
            parameter('bar) {
              get {
                service {
                  (request: HttpRequest[String]) => (bar: String) => Future(HttpResponse[JValue](content = Some(JString(bar))))
                }
              }
            }
          }
        }
      }

      handler.service(HttpRequest[String](HttpMethods.GET, "/foo/blahblah/entries")) must beLike {
        case Success(future) =>
          future must succeedWithContent {
            be_==(JString("blahblah"))
          }
      }
    }
  }

  "aggregate combinator" should {
    "aggregate full content when size is not specified" in {
      val handler = aggregate(None) {
        path("/foo") {
          get { (request: HttpRequest[ByteChunk]) =>
            Future(HttpResponse[ByteChunk](content = request.content))
          }
        }
      }

      handler.service(HttpRequest[ByteChunk](method = HttpMethods.GET, uri = "/foo", content = Some(Right(stream)))) must beLike {
        case Success(future) => future must succeedWithContent {
          (v: ByteChunk) => v must beLike {
            case Right(data) => Await.result(data.head.map(buf => buf.array.toList.take(buf.remaining)), Duration(2, "seconds")) must_== List[Byte]('1','2','3','4')
          }
        }
      }
    }

    "aggregate content up to the specified size" in {
      val handler = aggregate(Some(2.bytes)) {
        path("/foo") {
          get { (request: HttpRequest[ByteChunk]) =>
            Future(HttpResponse[ByteChunk](content = request.content))
          }
        }
      }

      handler.service(HttpRequest[ByteChunk](method = HttpMethods.GET, uri = "/foo", content = Some(Right(stream)))) must beLike {
        case Success(future) => future must succeedWithContent {
          (v: ByteChunk) => v must beLike {
            case Right(data) => Await.result(data.head.map(buf => buf.array.toList.take(buf.remaining)), Duration(2, "seconds")) must_== List[Byte]('1','2')

          }
        }
      }
    }
  }

  "decodeUrl combinator" should {
    "decode request URI" in {
      val svc = path("/foo/'bar") {
        produce[String, JValue, JValue](application / json) {
          decodeUrl {
            get { (request: HttpRequest[String]) =>
              Future(HttpResponse[JValue](content = Some(JString(request.uri.toString))))
            }
          }
        }
      }

      svc.service(HttpRequest[String](HttpMethods.GET, "/foo/blah%20blah")) must beLike {
        case Success(future) => future must succeedWithContent {
          be_==(JString("/foo/blah blah"))
        }
      }
    }
  }

  "produce combinator" should {
    val svc = path("/json") {
      produce[String, JValue, JValue](application / json) {
        get { (request: HttpRequest[String]) =>
          Future(HttpResponse[JValue](content = Some(JString("Good"))))
        }
      }
    }

    "return a proper content-type header" in {
      svc.service(HttpRequest[String](HttpMethods.GET, "/json")) must beLike {
        case Success(future) => future must whenDelivered {
          beLike {
            case HttpResponse(HttpStatus(OK, _), headers, _, _) => headers.get("Content-Type") must beSome("application/json")
          }
        }
      }
    }

    "not respond for incompatible Accept mime types" in {
      svc.service(HttpRequest[String](HttpMethods.GET, "/json", headers = HttpHeaders(Accept(MimeTypes.text / MimeTypes.csv) :: Nil))) must beLike {
        case Failure(Inapplicable(_)) => ok
      }
    }

    "respond to wildcard Accept mime types" in {
      svc.service(HttpRequest[String](HttpMethods.GET, "/json", headers = HttpHeaders(Accept(MimeTypes.parseMimeTypes("*/*"): _*) :: Nil))) must beLike {
        case Success(future) => future must whenDelivered {
          beLike {
            case HttpResponse(HttpStatus(OK, _), headers, _, _) => headers.get("Content-Type") must beSome("application/json")
          }
        }
      }

      svc.service(HttpRequest[String](HttpMethods.GET, "/json", headers = HttpHeaders(Accept(MimeTypes.parseMimeTypes("application/*"): _*) :: Nil))) must beLike {
        case Success(future) => future must whenDelivered {
          beLike {
            case HttpResponse(HttpStatus(OK, _), headers, _, _) => headers.get("Content-Type") must beSome("application/json")
          }
        }
      }

    }
  }
}
