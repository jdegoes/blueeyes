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
      } :: Nil)

      result must eventually (beEqualTo(Some("foo")))
    }
    "return Success when value is delivered" in{
      val result = futureDeliveryStrategy.deliver("foo", { s: String =>
      } :: Nil)

      result must eventually (beEqualTo(Success(())))
    }
    "return Failure with error when value is not delivered" in{
      val error = new NullPointerException()
      val result = futureDeliveryStrategy.deliver("foo", { s: String =>
        throw error
      } :: Nil)

      result must eventually (beEqualTo(Failure(error :: Nil)))
    }
  }

  private def futureDeliveryStrategy = strategy.futureDeliveryStrategy
}