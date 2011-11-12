package blueeyes.concurrent

import org.specs2.mutable.Specification
import blueeyes.util.metrics.Duration
import org.specs2.time.TimeConversions._
import java.util.concurrent.TimeUnit

class ScheduledExecutorSpec extends Specification{
  "ScheduledExecutor.once" should {
    "execute function" in{
      val f = ScheduledExecutor.once((a: Int) => Future.sync[Int](a), 1, Duration(10, TimeUnit.MILLISECONDS))

      f.value must eventually (beSome(1))
    }
  }
  "ScheduledExecutor.forever" should {
    "execute function while it is not cancelled" in{
      @volatile var exectionTime        = 0l
      @volatile var cancelled           = false
      @volatile var executedIfCancelled = false
      @volatile var executionCount      = 0

      val f = ScheduledExecutor.forever((a: Int) => {
        exectionTime   = System.currentTimeMillis
        executionCount = executionCount + 1
        if (cancelled) executedIfCancelled = true

        Future.sync[Int](a)
      }, 1, Duration(100, TimeUnit.MILLISECONDS))

      Thread.sleep(2000)

      f.cancel
      cancelled = true

      executedIfCancelled must eventually (be_==(false))

      executionCount must beGreaterThan(10)
      exectionTime   must beCloseTo(System.currentTimeMillis, 500)
    }
  }
  "ScheduledExecutor.repeat" should {
    "execute function specified times" in{
      @volatile var executionCount = 0

      val f = ScheduledExecutor.repeat((a: Int) => {
        executionCount = executionCount + 1
        Future.sync[Int](a)
      }, 1, Duration(20, TimeUnit.MILLISECONDS), 5)(15)((z: Int, a: Int) => z + a)


      f.value must eventually(5, 1000.milliseconds) (beSome(20))
      executionCount mustEqual(5)
    }
  }
  "ScheduledExecutor.repeatWhile" should {
    "execute function while condition is true" in{
      @volatile var executionCount = 0

      val f = ScheduledExecutor.repeatWhile((a: Int) => {
        executionCount = executionCount + 1
        Future.sync[Int](a)
      }, 1, Duration(20, TimeUnit.MILLISECONDS), (z: Int) => {z < 20})(15)((z: Int, a: Int) => z + a)


      f.value must eventually(5, 1000.milliseconds) (beSome(20))
      executionCount mustEqual(5)
    }
    "be cancelled" in{
      @volatile var executionCount = 0

      val f = ScheduledExecutor.repeatWhile((a: Int) => {
        executionCount = executionCount + 1
        Future.sync[Int](a)
      }, 1, Duration(200, TimeUnit.MILLISECONDS), (z: Int) => {z < 20})(15)((z: Int, a: Int) => z + a)

      Thread.sleep(200)
      f.cancel

      f.isCanceled must eventually(be_==(true))
      executionCount must lessThan(5)
    }
  }
}
