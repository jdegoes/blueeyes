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
  case class FutureTimeouts(retries: Int, duration: Duration)

  implicit val defaultFutureTimeouts: FutureTimeouts = FutureTimeouts(10, 100L.milliseconds)

  private sealed trait Outcome
  private case class Done(matchResult: (Boolean, String, String)) extends Outcome
  private case class Retry(failureMessage: String) extends Outcome

  /*
   analytics.get("/foo") must deliver {
     beSome(2)
   }
   */
  case class whenDelivered[A](inner: Matcher[A])(implicit timeouts: FutureTimeouts) extends Matcher[Future[A]]() {
    def apply(future: => Future[A]): (Boolean, String, String) = {
      println(timeouts)
      retry(future, timeouts.retries)
    }

    @tailrec
    private def retry(future: => Future[A], retries: Int): (Boolean, String, String) = {
      println("retries: " + retries)
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
            if (countedDown) Retry("Delivery of future was canceled on retry " + (timeouts.retries - retries) + ": " + delivered.error)
            else Retry("Retried after wait of " + timeouts.duration)
        }
      } catch {        
        case (_ : TimeoutException | _ : InterruptedException) => Retry("Delivery of future timed out")
        case ex: Throwable => 
          //ex.printStackTrace
          Retry("Got something weird: " + ex.getMessage)
      }  

      result match {
        case Done(result) => result

        case Retry(_) if (retries > 0) => {
          val end = System.currentTimeMillis
          Thread.sleep(timeouts.duration.milliseconds.length - (end - start))
          retry(future, retries - 1)
        }

        case Retry(message) => (false, "Enough timeout exceptions.", message)
      }
    }
  }
}
