package blueeyes.core.service;

import org.specs.Specification
import org.specs.util._
import blueeyes.core.data.{ Bijection, DataTranscoder }
import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import blueeyes.json.JsonParser
import blueeyes.json.JsonAST.JValue
import blueeyes.util.Future
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpHeaderImplicits._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.HttpNumberImplicits._
import blueeyes.core.http.{HttpMethod, HttpVersion, HttpMethods, HttpVersions, HttpRequest, HttpResponse, HttpStatusCode, HttpStatus, HttpStatusCodes, MimeType}

class HttpClientNettySpec extends Specification {
  val duration = 250
  val retries = 10
  val skip = true
  
  def skipper(): () => Unit = skip match {
    case true => skip("Will use Skalatra")
    case _ => () => Unit
  }
  
  "Support GET requests with status OK" in {
    skipper()()
    val f = new HttpClientNettyString().get("http://localhost/test/echo.php")
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.status.code must eventually(be(HttpStatusCodes.OK))
  }

  "Support GET requests with status Not Found" in {
    skipper()()
    val f = new HttpClientNettyString().get("http://localhost/bogus")
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.status.code must be(HttpStatusCodes.NotFound)
  }

  "Support GET requests with query params" in {
    skipper()()
    val f = new HttpClientNettyString()get("http://localhost/test/echo.php?param1=a&param2=b")
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with query params" in {
    skipper()()
    val f = new HttpClientNettyString().post("http://localhost/test/echo.php?param1=a&param2=b")
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with request params" in {
    skipper()()
    val f = new HttpClientNettyString().get("http://localhost/test/echo.php", 
                                            parameters=Map('param1 -> "a", 'param2 -> "b"))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with body" in {
    skipper()()
    val content = "Hello, world"
    val f = new HttpClientNettyString().post("http://localhost/test/echo.php", content=Some(content))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must eventually(equalIgnoreSpace(content))
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with body and request params" in {
    skipper()()
    val content = "Hello, world"
    val f = new HttpClientNettyString().post("http://localhost/test/echo.php", 
                                             content=Some(content), 
                                             parameters=Map('param1 -> "a", 'param2 -> "b"))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must equalIgnoreSpace("param1=a&param2=b" + content)
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support PUT requests with body" in {
    skipper()()
    val content = "Hello, world"
    val f = new HttpClientNettyString().put("http://localhost/test/echo.php", 
                                            content=Some(content), 
                                            headers=Map(`Content-Length`(100)))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support GET requests with header" in {
    skipper()()
    val f = new HttpClientNettyString().get("http://localhost/test/echo.php?headers=true", 
                                            headers=Map("Fooblahblah" -> "washere", "param2" -> "1"))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must include("Fooblahblah: washere")
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with Content-Type: text/html & Content-Length: 100" in {
    skipper()()
    val content = "<html></html>"
    val f = new HttpClientNettyString().post("http://localhost/test/echo.php", 
                                             content=Some(content), 
                                             headers=Map(`Content-Type`(text/html), `Content-Length`(100)))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must beEqual(content)
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with large payload" in {
    skipper()()
    val content = Array.fill(1024*1000)(0).toList.mkString("")
    val f = new HttpClientNettyString().post("http://localhost/test/echo.php", 
                                             content=Some(content))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must beEqual(content)
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support HEAD requests" in {
    skipper()()
    val f = new HttpClientNettyString().head("http://localhost/test/echo.php")
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support CONNECT requests" in {
    skip("CONNECT method TBD")
    val f = new HttpClientNettyString().connect("http://localhost/test/echo.php?headers=true", 
                                                headers=Map("Fooblahblah" -> "washere"))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with JValue" in {
    skipper()()
    val person = """
    { 
      "person": {
        "name": "John Doe",
        "age": 37,
        "spouse": {
          "person": {
            "name": "Jane Doe",
            "age": 35
          }
        }
      }
    }
    """
    val jvalue = JsonParser.parse(person)
    val f = new HttpClientNettyJValue().post("http://localhost/test/echo.php", content=Some(jvalue))
    f.deliverTo((res: HttpResponse[JValue]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.status.code must be(HttpStatusCodes.OK)
    f.value.get.content.get must beEqual(jvalue)
  }

  "Support POST requests with empty body" in {
    skipper()()
    val f = new HttpClientNettyJValue().post("http://localhost/test/echo.php")
    f.deliverTo((res: HttpResponse[JValue]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }
}

class HttpClientNettyString extends HttpClientNetty[String] with String2StringTranscoder

class HttpClientNettyJValue extends HttpClientNetty[JValue] with Json2StringTranscoder

trait Json2StringTranscoder {
  def transcode: Bijection[JValue, String] = new Bijection[JValue, String] {
    def apply(s: JValue)   = compact(render(s))
    def unapply(t: String) = JsonParser.parse(t)
  }
  def mimeType:MimeType = application/json
}

trait String2StringTranscoder {
  def transcode: Bijection[String, String] = new Bijection[String, String] {
    def apply(s: String) = s
    def unapply(t: String) = t
  }
  def mimeType:MimeType = text/plain
}

