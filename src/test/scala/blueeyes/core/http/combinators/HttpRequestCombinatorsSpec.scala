package blueeyes.core.http.combinators

import org.specs.Specification

import blueeyes.core.http.{HttpRequest, HttpResponse, HttpException, HttpStatus}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.HttpMethods._
import blueeyes.util.Future
import blueeyes.json.JsonAST._

class HttpRequestCombinatorsSpec extends Specification with HttpRequestCombinators {
  type Handler[T, S] = HttpRequest[T] => Future[HttpResponse[S]]
  
  "refineContentType should throw an exception when type cannot be refined to specified subtype" in {
    jObjectCaller { 
      refineContentType { jIntHandler }
    }.value.get must throwAnException[HttpException]
  }
  
  "refineContentType should refine content type when possible" in {
    jIntCaller { 
      refineContentType { jIntHandler }
    }.value.get.content.get mustEqual(JInt(123))
  }
  
  
  
  def jObjectCaller(h: Handler[JValue, JValue]) = h(HttpRequest(uri = "/", method = GET, content = Some(JObject(Nil))))
  
  def jIntCaller(h: Handler[JValue, JValue]) = h(HttpRequest(uri = "/", method = GET, content = Some(JInt(123))))
  
  def jIntHandler(r: HttpRequest[JInt]): Future[HttpResponse[JValue]] = Future(HttpResponse(content = Some(JInt(123): JValue)))
}