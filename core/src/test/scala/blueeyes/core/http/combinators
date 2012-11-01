package blueeyes.core.http.combinators

import org.specs2.mutable.Specification

import blueeyes.core.http.{HttpRequest, HttpResponse, HttpException, HttpStatus}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.HttpMethods._
import blueeyes.core.http.test.HttpRequestMatchers
import blueeyes.json._
import akka.dispatch.Future

class HttpRequestCombinatorsSpec extends Specification with HttpRequestCombinators with HttpRequestMatchers with blueeyes.bkka.AkkaDefaults {
  type Handler[T, S] = HttpRequest[Future[T]] => Future[HttpResponse[S]]
  
  "refineContentType should return bad request type cannot be refined to specified subtype" in {
    jObjectCaller { refineContentType { jIntHandler } } must respondWithCode(BadRequest)
  }
  
  "refineContentType should refine content type when possible" in {
    jIntCaller { refineContentType { jIntHandler } } must succeedWithContent(be_==(JNum(123)))
  }
  
  def jObjectCaller(h: Handler[JValue, JValue]) = h(HttpRequest(uri = "/", method = GET, content = Some(Future(JObject(Nil)))))
  
  def jIntCaller(h: Handler[JValue, JValue]) = h(HttpRequest(uri = "/", method = GET, content = Some(Future(JNum(123)))))
  
  def jIntHandler(r: HttpRequest[Future[JNum]]): Future[HttpResponse[JValue]] = Future(HttpResponse(content = Some(JNum(123): JValue)))
}
