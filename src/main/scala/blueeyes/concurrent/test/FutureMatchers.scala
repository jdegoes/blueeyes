package blueeyes.concurrent
package test

import scala.annotation.tailrec  
import org.specs2.execute.FailureException
import java.util.concurrent.{TimeoutException,  CountDownLatch}

import blueeyes.util.metrics.Duration
import blueeyes.util.metrics.Duration._
import blueeyes.util.RichThrowableImplicits._
import org.specs2.matcher._

trait FutureMatchers { self: MustExpectations =>
  case class FutureTimeouts(retries: Int, duration: Duration)

  implicit val defaultFutureTimeouts: FutureTimeouts = FutureTimeouts(10, 100L.milliseconds)

  private sealed trait Outcome[A]
  private case class Done[A](matchResult: MatchResult[A]) extends Outcome[A]
  private case class Retry[A](failureMessage: String) extends Outcome[A]

  /*
   analytics.get("/foo") must whenDelivered {
     beSome(2)
   }
   */
  case class whenDelivered[A, V](inner: (A) => MatchResult[V])(implicit timeouts: FutureTimeouts) extends Matcher[Future[A]] with AnyMatchers{
    private val eventuallyMatcher = new EventuallyMatchers{}
    def apply[S <: Future[A]](t: Expectable[S]): MatchResult[S] = {
      checkFailure(retry(t, timeouts.retries, timeouts.retries))
    }

    @tailrec
    private def retry[S <: Future[A]](t: Expectable[S], retries: Int, totalRetries: Int): MatchResult[S] = {
      import org.specs2.time.TimeConversions._
      val start = System.currentTimeMillis

      val delivered = eventuallyMatcher.eventually(1, 1.seconds)(not (beNull))(t).expectable.value

      val latch = new CountDownLatch(1)

      delivered.deliverTo(_ => latch.countDown())
      delivered.ifCanceled(_ => latch.countDown())

      val test: Outcome[S] = try {
        val countedDown = latch.await(timeouts.duration.length, timeouts.duration.unit)

        delivered.value match {
          case Some(value) => 
            val test = inner(value)

            if (test.isSuccess || retries <= 0) Done(result[S](test, t))
            else Retry(test match{
              case f @ MatchFailure(ok, ko, _, _) => ko
              case f @ MatchSkip(m, _)            => m
              case _ => test.message
            })

          case None => 
            if (countedDown) Retry("Delivery of future was canceled on retry " + (timeouts.retries - retries) + ": " + delivered.error.map(_.fullStackTrace))
            else Retry("Retried " + (totalRetries - retries) + " times with interval of " + timeouts.duration + " but did not observe a result being returned.")
        }
      } catch {        
        case (_ : TimeoutException | _ : InterruptedException) => Retry("Delivery of future timed out")
        case failure: FailureException => Retry("Assertion failed on retry " + (totalRetries - retries) + ": " + failure.getMessage)
        case ex: Throwable => Retry("Caught exception in matching delivered value: " + ex.fullStackTrace)
      }  

      test match {
        case Done(test) => test

        case Retry(_) if (retries > 0) => {
          val end = System.currentTimeMillis
          Thread.sleep(0L.max(timeouts.duration.milliseconds.length - (end - start)))
          print(".")
          retry(t, retries - 1, totalRetries)
        }

        case Retry(message) =>
          result[S](false, "Enough timeout exceptions.", message, t)
      }
    }
  }
}
