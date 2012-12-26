package blueeyes.core.http.combinators

import blueeyes.core.http.{HttpRequest, HttpResponse, HttpException, HttpStatus}
import blueeyes.core.http.HttpStatusCodes._
import akka.dispatch.Future
import akka.dispatch.ExecutionContext

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
  private type Handler2[T, S, E1] = HttpRequest[Future[T]] => E1 => Future[HttpResponse[S]]

  def refineContentType[S >: T, T](f: Handler[T, S])(implicit m: Manifest[T], executor: ExecutionContext): Handler[S, S] = (request: HttpRequest[Future[S]]) => refineContentType(request, f)

  def refineContentType2[S >: T, T, E1](f: Handler2[T, S, E1])(implicit m: Manifest[T], executor: ExecutionContext): Handler2[S, S, E1] =
    (request: HttpRequest[Future[S]]) => (e: E1) => refineContentType(request, (r: HttpRequest[Future[T]]) => f(r)(e))

  def requireContent[T, S](p: T => Boolean)(f: Handler[T, S])(implicit m: Manifest[T], executor: ExecutionContext): Handler[T, S] = (request: HttpRequest[Future[T]]) => requireContent(p, request, f)

  def requireContent2[T, S, E1](p: T => Boolean)(f: Handler2[T, S, E1])(implicit m: Manifest[T], executor: ExecutionContext): Handler2[T, S, E1] =
    (request: HttpRequest[Future[T]]) => (e: E1) => requireContent(p, request, (r: HttpRequest[Future[T]]) => f(r)(e))

  private def refineContentType[S >: T, T](request: HttpRequest[Future[S]], f: Handler[T, S])(implicit m: Manifest[T], executor: ExecutionContext): Future[HttpResponse[S]] = {
    request.content match {
      case None =>
        Future(HttpResponse(HttpStatus(BadRequest, "Expected " + m.erasure.getClass.getName + " but found nothing")))

      case Some(future) => future.flatMap{ value =>
        if (m.erasure.isAssignableFrom(value.getClass)) {
          val t: T = value.asInstanceOf[T]

          f(request.copy(content = Some(Future(t))))
        }
        else Future(HttpResponse(HttpStatus(BadRequest, "Expected " + m.erasure.getClass.getName + " but found: " + value.getClass.getName)))
      }
    }
  }

  private def requireContent[T, S](p: T => Boolean, request: HttpRequest[Future[T]], f: Handler[T, S])(implicit m: Manifest[T], executor: ExecutionContext): Future[HttpResponse[S]] = {
    request.content match {
      case None =>
        throw HttpException(BadRequest, "Expected " + m.erasure.getClass.getName + " but found nothing")

      case Some(future) => future.flatMap{ value =>
        if (p(value)) f(request) else Future(HttpResponse(HttpStatus(BadRequest)))
      }
    }
  }
}

object HttpRequestCombinators extends HttpRequestCombinators
