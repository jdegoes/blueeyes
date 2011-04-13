package blueeyes.concurrent

import org.specs.Specification
import org.specs.util.{Duration => SpecsDuration}
import Duration._

class ScheduledExecutorSpec extends Specification with FutureDeliveryStrategySequential{
  "ScheduledExecutor.once" should {
    "execute function" in{
      val f = ScheduledExecutor.once((a: Int) => Future.lift[Int](a), 1, 10.milliseconds)

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

        Future.lift[Int](a)
      }, 1, 100.milliseconds)

      Thread.sleep(2000)

      f.cancel
      cancelled = true

      val lastExecutionTime = System.currentTimeMillis

      Thread.sleep(500)

      executedIfCancelled must be (false)
      executionCount must beGreaterThan(10)
      exectionTime   must beCloseTo(lastExecutionTime, 200)
    }
  }
  "ScheduledExecutor.repeat" should {
    "execute function specified times" in{
      @volatile var executionCount = 0

      val f = ScheduledExecutor.repeat((a: Int) => {
        executionCount = executionCount + 1
        Future.lift[Int](a)
      }, 1, 20.milliseconds, 5)(15)((z: Int, a: Int) => z + a)


      f.value must eventually(5, new SpecsDuration(1000)) (beSome(20))
      executionCount mustEqual(5)
    }
  }
  "ScheduledExecutor.repeatWhile" should {
    "execute function while condition is true" in{
      @volatile var executionCount = 0

      val f = ScheduledExecutor.repeatWhile((a: Int) => {
        executionCount = executionCount + 1
        Future.lift[Int](a)
      }, 1, 20.milliseconds, (z: Int) => {z < 20})(15)((z: Int, a: Int) => z + a)


      f.value must eventually(5, new SpecsDuration(1000)) (beSome(20))
      executionCount mustEqual(5)
    }
    "be cancelled" in{
      @volatile var executionCount = 0

      val f = ScheduledExecutor.repeatWhile((a: Int) => {
        executionCount = executionCount + 1
        Future.lift[Int](a)
      }, 1, 200.milliseconds, (z: Int) => {z < 20})(15)((z: Int, a: Int) => z + a)

      Thread.sleep(200)
      f.cancel

      f.isCanceled must eventually(be(true))
      executionCount must lessThan(5)
    }
  }
}