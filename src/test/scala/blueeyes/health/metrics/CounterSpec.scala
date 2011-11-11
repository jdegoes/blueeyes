package blueeyes.health.metrics

import blueeyes.json.JsonAST.JInt
import org.specs2.mutable.Specification

class CounterSpec extends Specification {
  "a counter of zero" should {
    def makeCounter = new Counter(0)

    "incremented by one" in {
      val counter = makeCounter
      counter  += 1

      counter.count mustEqual(1)
    }

    "incremented by two" in {
      val counter = makeCounter
      counter += 2

      counter.count mustEqual(2)
    }

    "composes Counter" in{
      val counter = new Counter(0)
      counter += 2

      counter.toJValue mustEqual(JInt(2))
    }
  }

  "a counter without an explicit initial value" should {
    "equals one" in {
      new Counter().count mustEqual(0)
    }
  }
}