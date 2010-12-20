package blueeyes.health.metrics

import org.scalatest.matchers.MustMatchers
import org.scalatest.Spec
import blueeyes.json.JsonAST.JInt

class CounterTest extends Spec with MustMatchers {
  describe("a counter of zero") {
    def makeCounter = new Counter(0)

    describe("incremented by one") {
      val counter = makeCounter
      counter  += 1

      it(" equals one") {
        counter.count must equal(1)
      }
    }

    describe("incremented by two") {
      val counter = makeCounter
      counter += 2

      it("equals two") {
        counter.count must equal(2)
      }
    }

    describe("composes Counter"){
      val counter = new Counter(0)
      counter += 2

      counter.toJValue must equal(JInt(2))
    }
  }

  describe("a counter without an explicit initial value") {
    it("equals one") {
      new Counter().count must equal(0)
    }
  }
}