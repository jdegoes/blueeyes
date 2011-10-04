package blueeyes.core.http.combinators

import blueeyes.core.http.{HttpRequest, HttpResponse, HttpException, HttpStatus}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.concurrent.Future

/**
 *
 * <pre>
 * post {
 *   refineContentType[JObject] {
 *     requireContent(!(_ \ "adId" -->? classOf[JString]).isEmpty) { request =>
 *       val adId = (request.content \ "adId").deserialize[String]
 *     }
 *   }
 * }
 * </pre>
 *
 */
trait HttpRequestCombinators {
  private type Handler[T, S] = HttpRequest[Future[T]] => Future[HttpResponse[S]]

  def refineContentType[S >: T, T](f: Handler[T, S])(implicit m: Manifest[T]): Handler[S, S] = {
    (request: HttpRequest[Future[S]]) => {
      request.content match {
        case None => 
          Future.sync(HttpResponse(HttpStatus(BadRequest, "Expected " + m.erasure.getClass.getName + " but found nothing")))

        case Some(future) => future.flatMap{ value =>
          if (value.getClass == m.erasure) {
            val t: T = value.asInstanceOf[T]

            f(request.copy(content = Some(Future.sync(t))))
          }
          else Future.sync(HttpResponse(HttpStatus(BadRequest, "Expected " + m.erasure.getClass.getName + " but found: " + value.getClass.getName)))
        }
      }
    }
  }

  def requireContent[T, S](p: T => Boolean)(f: Handler[T, S])(implicit m: Manifest[T]): Handler[T, S] = {
    (request: HttpRequest[Future[T]]) => {
      request.content match {
        case None => 
          throw HttpException(BadRequest, "Expected " + m.erasure.getClass.getName + " but found nothing")

        case Some(future) => future.flatMap{ value =>
          if (p(value)) f(request) else Future.sync(HttpResponse(HttpStatus(BadRequest)))
        }
      }
    }
  }
}
object HttpRequestCombinators extends HttpRequestCombinators
