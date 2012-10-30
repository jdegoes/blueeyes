import scala.annotation.tailrec  
import org.specs2.execute.FailureException
import java.util.concurrent.{TimeoutException,  CountDownLatch}

import akka.dispatch.Future
import akka.dispatch.Await
import akka.util.Duration
import akka.util.DurationLong
import akka.util.duration._

import blueeyes.util.RichThrowableImplicits._

import org.specs2.matcher._

package blueeyes.concurrent.test {

trait AkkaConversions {
  implicit def specsDuration2Akka(duration: org.specs2.time.Duration): akka.util.Duration = new DurationLong(duration.inMillis).millis
  implicit def specsDuration2Rich(duration: org.specs2.time.Duration) = new RichSpecsDuration(duration)

  class RichSpecsDuration(duration: org.specs2.time.Duration) {
    def toAkka = specsDuration2Akka(duration)
  }
}

trait FutureMatchers extends AkkaConversions { 
  case class FutureTimeouts(retries: Int, duration: Duration)

  private sealed trait Outcome[A]
  private case class Done[A](matchResult: MatchResult[A]) extends Outcome[A]
  private case class Retry[A](failureMessage: String) extends Outcome[A]

  implicit val defaultFutureTimeouts: FutureTimeouts = FutureTimeouts(10, 100L millis)

  case class whenDelivered[A](matcher: Matcher[A])(implicit timeouts: FutureTimeouts) extends Matcher[Future[A]] with Expectations {
    def apply[B <: Future[A]](expectable: Expectable[B]): MatchResult[B] = {
      val (ok, okMessage, koMessage) = retry(expectable.evaluate.value, timeouts.retries, timeouts.retries)
      result(ok, okMessage, koMessage, expectable)
    }

    @tailrec
    private def retry[B <: Future[A]](future: => B, retries: Int, totalRetries: Int): (Boolean, String, String) = {
      val start = System.currentTimeMillis
      
      val outcome: Outcome[A] = try {
        val result = Await.result(future, timeouts.duration)
        val protoResult = matcher(result aka "The value returned from the Future")

        if (protoResult.isSuccess || retries <= 0) Done(protoResult)
        else protoResult match{
          case f @ MatchFailure(ok, ko, _, _) => Retry(ko())
          case f @ MatchSkip(m, _)            => Retry(m)
          case _ => Retry(protoResult.message)
        }
      } catch {
        case timeout: TimeoutException => Retry("Retried " + (totalRetries - retries) + " times with interval of " + timeouts.duration + " but did not observe a result.")
        case failure: FailureException => Retry("Assertion failed on retry " + (totalRetries - retries) + ": " + failure.f.message)
        case ex: Throwable             => Retry("Delivery of future was canceled on retry " + (timeouts.retries - retries) + ": " + ex.fullStackTrace)
      }  

      outcome match {
        case Done(result) => (result.isSuccess, result.message, result.message)

        case Retry(_) if (retries > 0) => 
          val end = System.currentTimeMillis
          Thread.sleep(0L.max(timeouts.duration.toMillis - (end - start)))
          print(".")
          retry(future, retries - 1, totalRetries)

        case Retry(message) => (false, "This message should never be seen.", message)
      }
    }
  }
}}
