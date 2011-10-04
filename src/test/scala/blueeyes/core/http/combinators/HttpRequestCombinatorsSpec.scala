package blueeyes.core.http.combinators

import org.specs.Specification

import blueeyes.core.http.{HttpRequest, HttpResponse, HttpException, HttpStatus}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.HttpMethods._
import blueeyes.json.JsonAST._
import blueeyes.concurrent.Future

class HttpRequestCombinatorsSpec extends Specification with HttpRequestCombinators{
  type Handler[T, S] = HttpRequest[Future[T]] => Future[HttpResponse[S]]
  
  "refineContentType should return bad request type cannot be refined to specified subtype" in {
    jObjectCaller { 
      refineContentType { jIntHandler }
    }.value.get.status.code mustEqual(BadRequest)
  }
  
  "refineContentType should refine content type when possible" in {
    jIntCaller { 
      refineContentType { jIntHandler }
    }.value.get.content.get mustEqual(JInt(123))
  }
  
  
  
  def jObjectCaller(h: Handler[JValue, JValue]) = h(HttpRequest(uri = "/", method = GET, content = Some(Future.sync(JObject(Nil)))))
  
  def jIntCaller(h: Handler[JValue, JValue]) = h(HttpRequest(uri = "/", method = GET, content = Some(Future.sync(JInt(123)))))
  
  def jIntHandler(r: HttpRequest[Future[JInt]]): Future[HttpResponse[JValue]] = Future.sync(HttpResponse(content = Some(JInt(123): JValue)))
}
