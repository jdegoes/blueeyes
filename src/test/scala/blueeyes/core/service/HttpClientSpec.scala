package blueeyes.core.service;

import MimeTypes._
import blueeyes.core.data.{Bijection, DataTranscoder}
import blueeyes.util.Future
import org.specs.Specification
import org.specs.util._

class HttpClientSpec extends Specification {
  "Support get requests with status OK" in {
	skip("Will use Skalatra")
    val f = new HttpClientNettyString()(HttpRequest(HttpMethods.GET, "http://localhost"))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(beSomething) 
    f.value.get.status.code must eventually(20, new Duration(500))(be(HttpStatusCodes.OK))
  } 

  "Support get requests with status Not Found" in {
	skip("Will use Skalatra")
    val f = new HttpClientNettyString()(HttpRequest(HttpMethods.GET, "http://localhost/bogus"))
    f.deliverTo((res: HttpResponse[String]) => {})
    f.value must eventually(beSomething) 
    f.value.get.status.code must eventually(20, new Duration(500))(be(HttpStatusCodes.NotFound))
  }
}

class HttpClientNettyString extends HttpClientNetty[String] with String2StringTranscoder 

trait String2StringTranscoder extends DataTranscoder[String, String] {
  def transcode: Bijection[String, String] = new Bijection[String, String] {
    def apply(s: String) = s
    def unapply(t: String) = t
  }
  def mimeType = text/plain
}
