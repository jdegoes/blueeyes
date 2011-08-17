package blueeyes.concurrent
package test

import scala.annotation.tailrec  
import org.specs.matcher.Matcher
import org.specs.specification.FailureExceptionWithResult

import java.util.concurrent.{TimeoutException,  CountDownLatch}

import blueeyes.util.metrics.Duration
import blueeyes.util.metrics.Duration._
import blueeyes.util.RichThrowableImplicits._

trait FutureMatchers {
  case class FutureTimeouts(retries: Int, duration: Duration)

  implicit val defaultFutureTimeouts: FutureTimeouts = FutureTimeouts(10, 100L.milliseconds)

  private sealed trait Outcome
  private case class Done(matchResult: (Boolean, String, String)) extends Outcome
  private case class Retry(failureMessage: String) extends Outcome

  /*
   analytics.get("/foo") must whenDelivered {
     beSome(2)
   }
   */
  case class whenDelivered[A](inner: Matcher[A])(implicit timeouts: FutureTimeouts) extends Matcher[Future[A]]() {
    def apply(future: => Future[A]): (Boolean, String, String) = {
      retry(future, timeouts.retries, timeouts.retries)
    }

    @tailrec
    private def retry(future: => Future[A], retries: Int, totalRetries: Int): (Boolean, String, String) = {
      val start = System.currentTimeMillis

      val delivered = future

      val latch = new CountDownLatch(1)

      delivered.deliverTo(_ => latch.countDown())
      delivered.ifCanceled(_ => latch.countDown())

      val result = try {
        val countedDown = latch.await(timeouts.duration.length, timeouts.duration.unit)

        delivered.value match {
          case Some(value) => 
            val result = inner(value) 

            if (result.success || retries <= 0) Done(result)
            else Retry(result.koMessage)

          case None => 
            if (countedDown) Retry("Delivery of future was canceled on retry " + (timeouts.retries - retries) + ": " + delivered.error.map(_.fullStackTrace))
            else Retry("Retried " + (totalRetries - retries) + " times with interval of " + timeouts.duration + " but did not observe a result being returned.")
        }
      } catch {        
        case (_ : TimeoutException | _ : InterruptedException) => Retry("Delivery of future timed out")
        case failure: FailureExceptionWithResult[_] => Retry("Assertion failed on retry " + (totalRetries - retries) + ": " + failure.getMessage)
        case ex: Throwable => Retry("Caught exception in matching delivered value: " + ex.fullStackTrace)
      }  

      result match {
        case Done(result) => result

        case Retry(_) if (retries > 0) => {
          val end = System.currentTimeMillis
          Thread.sleep(0L.max(timeouts.duration.milliseconds.length - (end - start)))
          print(".")
          retry(future, retries - 1, totalRetries)
        }

        case Retry(message) => 
          println
          (false, "Enough timeout exceptions.", message)
      }
    }
  }
}
