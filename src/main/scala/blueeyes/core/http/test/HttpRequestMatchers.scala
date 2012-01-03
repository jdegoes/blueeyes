package blueeyes.core.http
package test

import blueeyes.concurrent.test.FutureMatchers
import org.specs2.mutable.Specification
import org.specs2.matcher._
import akka.dispatch.Future

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
