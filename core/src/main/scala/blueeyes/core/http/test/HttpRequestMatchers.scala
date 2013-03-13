package blueeyes.core.http
package test

import blueeyes.akka_testing.FutureMatchers
import org.specs2.mutable.Specification
import org.specs2.matcher._
import akka.dispatch.Future
import akka.dispatch.ExecutionContext

trait HttpRequestMatchers extends Specification with FutureMatchers {
  case class succeedWithContent[A](matcher: Matcher[A]) extends Matcher[Future[HttpResponse[A]]] {
    def apply[B <: Future[HttpResponse[A]]](expectable: Expectable[B]): MatchResult[B] = {
      val nested = whenDelivered[HttpResponse[A]] {
        beLike {
          case HttpResponse(status, _, Some(content), _) => 
            (status.code must_== HttpStatusCodes.OK) and
            (matcher(content aka "The content of the response"))
        }
      }

      nested(expectable)
    }
  }

  def succeedWithFutureContent[A](matcher: Matcher[A])(implicit executor: ExecutionContext): Matcher[Future[HttpResponse[Future[A]]]] = {
    succeedWithContent(matcher) ^^ { 
      (_: Future[HttpResponse[Future[A]]]).flatMap { response =>
        response.content match {
          case Some(content) => content map { c => response.copy(content = Some(c)) }
          case None => Future(response.copy(content = None))
        }
      }
    }
  }

  case class respondWithCode[A](expected: HttpStatusCode) extends Matcher[Future[HttpResponse[A]]] {
    def apply[B <: Future[HttpResponse[A]]](expectable: Expectable[B]): MatchResult[B] = {
      val nested = whenDelivered[HttpResponse[A]] {
        beLike {
          case HttpResponse(status, _, _, _) => status.code must_== expected
        }
      }

      nested(expectable)
    }
  }
}

// vim: set ts=4 sw=4 et:
