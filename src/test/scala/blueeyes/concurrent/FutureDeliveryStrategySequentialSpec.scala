package blueeyes.concurrent

import scalaz.{Success, Failure}
import org.specs.Specification

class FutureDeliveryStrategySequentialSpec extends Specification{
  private val strategy = new FutureDeliveryStrategySequential{}

  "FutureDeliveryStrategySequential.deliver" should{
    "deliver value to listener" in{
      var result: Option[String] = None

      futureDeliveryStrategy.deliver("foo", { s: String =>
        result = Some(s)
      } :: Nil, {errors => })

      result must eventually (beEqualTo(Some("foo")))
    }
    "deliver errors with error handler when value is not delivered" in{
      var delivered = false
      val error = new NullPointerException()
      val result = futureDeliveryStrategy.deliver("foo", { s: String =>
        throw error
      } :: Nil, {errors => delivered = true})

      delivered must eventually (be(true))
    }
  }

  private def futureDeliveryStrategy = strategy.futureDeliveryStrategy
}