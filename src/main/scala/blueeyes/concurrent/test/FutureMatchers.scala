package blueeyes.concurrent
package test

import scala.annotation.tailrec  
import org.specs2.execute.FailureException
import java.util.concurrent.{TimeoutException,  CountDownLatch}

import blueeyes.concurrent._
import blueeyes.util.metrics.Duration
import blueeyes.util.metrics.Duration._
import blueeyes.util.RichThrowableImplicits._

import org.specs2.matcher._

trait FutureMatchers { 
  case class FutureTimeouts(retries: Int, duration: Duration)

  private sealed trait Outcome[A]
  private case class Done[A](matchResult: MatchResult[A]) extends Outcome[A]
  private case class Retry[A](failureMessage: String) extends Outcome[A]

  implicit val defaultFutureTimeouts: FutureTimeouts = FutureTimeouts(10, 100L.milliseconds)

  case class whenDelivered[A](matcher: Matcher[A])(implicit timeouts: FutureTimeouts) extends Matcher[Future[A]] with Expectations {
    def apply[B <: Future[A]](expectable: Expectable[B]): MatchResult[B] = {
      val (ok, okMessage, koMessage) = retry(expectable.value, timeouts.retries, timeouts.retries)
      result(ok, okMessage, koMessage, expectable)
    }

    @tailrec
    private def retry[B <: Future[A]](future: => B, retries: Int, totalRetries: Int): (Boolean, String, String) = {
      import org.specs2.time.TimeConversions._
      val start = System.currentTimeMillis
      val delivered = future

      val latch = new CountDownLatch(1)
      delivered.deliverTo(_ => latch.countDown())
      delivered.ifCanceled(_ => latch.countDown())

      val outcome: Outcome[A] = try {
        val countedDown = latch.await(timeouts.duration.length, timeouts.duration.unit)

        delivered.value match {
          case Some(value) => 
            val protoResult = matcher(value aka "The value returned from the Future")

            if (protoResult.isSuccess || retries <= 0) Done(protoResult)
            else Retry(protoResult match{
              case f @ MatchFailure(ok, ko, _, _) => ko
              case f @ MatchSkip(m, _)            => m
              case _ => protoResult.message
            })

          case None => 
            if (countedDown) Retry("Delivery of future was canceled on retry " + (timeouts.retries - retries) + ": " + delivered.error.map(_.fullStackTrace))
            else Retry("Retried " + (totalRetries - retries) + " times with interval of " + timeouts.duration + " but did not observe a result being returned.")
        }
      } catch {        
        case (_ : TimeoutException | _ : InterruptedException) => Retry("Delivery of future timed out")
        case failure: FailureException => Retry("Assertion failed on retry " + (totalRetries - retries) + ": " + failure.f.message)
        case ex: Throwable => Retry("Caught exception in matching delivered value: " + ex.fullStackTrace)
      }  

      outcome match {
        case Done(result) => (result.isSuccess, result.message, result.message)

        case Retry(_) if (retries > 0) => 
          val end = System.currentTimeMillis
          Thread.sleep(0L.max(timeouts.duration.milliseconds.length - (end - start)))
          print(".")
          retry(future, retries - 1, totalRetries)

        case Retry(message) => (false, "This message should never be seen.", message)
      }
    }
  }
}
