package blueeyes.core.service;

import HttpHeaders._
import HttpHeaderImplicits._
import MimeTypes._
import blueeyes.core.data.{ Bijection, DataTranscoder }
import blueeyes.util.Future
import org.specs.Specification
import org.specs.util._

class HttpClientSpec extends Specification {
  val duration = 250
  val retries = 10

  "Support GET requests with status OK" in {
    skip("Will use Skalatra")
    val f = new HttpClientNettyString()(HttpRequest(HttpMethods.GET, "http://localhost/test/echo.php"))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.status.code must eventually(be(HttpStatusCodes.OK))
  }

  "Support GET requests with status Not Found" in {
    skip("Will use Skalatra")
    val f = new HttpClientNettyString()(HttpRequest(HttpMethods.GET, "http://localhost/bogus"))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.status.code must be(HttpStatusCodes.NotFound)
  }

  "Support GET requests with query params" in {
    skip("Will use Skalatra")
    val f = new HttpClientNettyString()(HttpRequest(HttpMethods.GET, "http://localhost/test/echo.php?param1=a&param2=b"))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with query params" in {
    skip("Will use Skalatra")
    val f = new HttpClientNettyString()(HttpRequest(HttpMethods.POST, "http://localhost/test/echo.php?param1=a&param2=b"))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with request params" in {
    skip("Will use Skalatra")
    val f = new HttpClientNettyString()(HttpRequest(HttpMethods.POST, "http://localhost/test/echo.php", parameters=Map('param1 -> "a", 'param2 -> "b")))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must eventually(equalIgnoreSpace("param1=a&param2=b"))
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with body" in {
    skip("Will use Skalatra")
    val content = "Hello, world"
    val f = new HttpClientNettyString()(HttpRequest(HttpMethods.POST, "http://localhost/test/echo.php", content=Some(content)))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must eventually(equalIgnoreSpace(content))
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support POST requests with body and request params" in {
    skip("Will use Skalatra")
    val content = "Hello, world"
    val f = new HttpClientNettyString()(HttpRequest(HttpMethods.POST, "http://localhost/test/echo.php", content=Some(content), parameters=Map('param1 -> "a", 'param2 -> "b")))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must equalIgnoreSpace("param1=a&param2=b" + content)
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }

  "Support GET requests with header" in {
    skip("Will use Skalatra")
    val f = new HttpClientNettyString()(HttpRequest(HttpMethods.GET, "http://localhost/test/echo.php?headers", headers=Map("Fooblahblah" -> "washere")))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(retries, new Duration(duration))(beSomething)
    f.value.get.content.get.trim must equalIgnoreSpace("Fooblahblah: washere")
    f.value.get.status.code must be(HttpStatusCodes.OK)
  }
}

class HttpClientNettyString extends HttpClientNetty[String] with String2StringTranscoder

trait String2StringTranscoder extends DataTranscoder[String, String] {
  def transcode: Bijection[String, String] = new Bijection[String, String] {
    def apply(s: String) = s
    def unapply(t: String) = t
  }
  def mimeType:MimeType = text/plain
}
