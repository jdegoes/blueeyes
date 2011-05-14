package blueeyes.concurrent
package test

import scala.annotation.tailrec  
import org.specs.matcher.{Matcher}

import java.util.concurrent.{TimeoutException,  TimeUnit, CountDownLatch}

import blueeyes.concurrent.Duration
import blueeyes.concurrent.Duration._

trait FutureMatchers {
  case class FutureTimeouts(retries: Int, timeout: Duration)

  implicit val defaultFutureTimeouts: FutureTimeouts = FutureTimeouts(10, 100L.milliseconds)

  /*
   analytics.get("/foo") must deliver {
     beSome(2)
   }
   */
  case class deliver[A](inner: Matcher[Option[A]])(implicit timeouts: FutureTimeouts) extends Matcher[Future[A]]() {
    def apply(future: => Future[A]): (Boolean, String, String) = {
      retry(future, timeouts.retries)
    }

    @tailrec
    private def retry(future: => Future[A], retries: Int): (Boolean, String, String) = {
      val delivered = future

      val latch = new CountDownLatch(1)

      delivered.deliverTo(_ => latch.countDown())

      val result = try {
        latch.await(timeouts.timeout.time, timeouts.timeout.unit)
        Right(inner(delivered.value))
      } catch {        
        case (_ : TimeoutException | _ : InterruptedException) => Left(retries - 1)
      }  

      result match {
        case Right(result) if (result.success || retries <= 0) => result
        case Left(toRetry) if (toRetry > 0) => retry(future, toRetry)        
        case _ => (false, "Too many timeout exceptions.", "Not enough timeout exceptions.")
      }
    }
  }
}