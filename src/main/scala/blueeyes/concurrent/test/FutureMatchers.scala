package blueeyes.concurrent
package test

import scala.annotation.tailrec  
import org.specs.matcher.{Matcher}

import java.util.concurrent.{TimeoutException,  TimeUnit, CountDownLatch}

import blueeyes.concurrent.Duration
import blueeyes.concurrent.Duration._

import scalaz.Scalaz._
import scalaz.{Validation, Success, Failure}

trait FutureMatchers {
  case class FutureTimeouts(retries: Int, timeout: Duration)

  implicit val defaultFutureTimeouts: FutureTimeouts = FutureTimeouts(10, 100L.milliseconds)

  /*
   analytics.get("/foo") must deliver {
     beSome(2)
   }
   */
  case class whenDelivered[A](inner: Matcher[Option[A]])(implicit timeouts: FutureTimeouts) extends Matcher[Future[A]]() {
    def apply(future: => Future[A]): (Boolean, String, String) = {
      retry(future, timeouts.retries)
    }

    @tailrec
    private def retry(future: => Future[A], retries: Int): (Boolean, String, String) = {
      val delivered = future

      val latch = new CountDownLatch(1)

      delivered.deliverTo(_ => latch.countDown())

      val result = try {
        latch.await(timeouts.timeout.time.toLong, timeouts.timeout.unit)

        inner(delivered.value) |> { result =>
          if (result.success || retries <= 0) success(result)
          else failure(result.koMessage)
        }
      } catch {        
        case (_ : TimeoutException | _ : InterruptedException) => failure("Delivery of future timed out")
      }  

      result match {
        case Success(result) => result

        case Failure(_) if (retries > 0) => retry(future, retries - 1)

        case Failure(message) => (false, "Enough timeout exceptions.", message)
      }
    }
  }
}
