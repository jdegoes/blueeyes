package blueeyes.persistence.cache

import org.specs.Specification
import java.util.concurrent.TimeUnit.{MILLISECONDS}

import scalaz.Semigroup

class StageSpec extends Specification{
  implicit val StringSemigroup = new Semigroup[String] {
    def append(s1: String, s2: => String) = s1 + "-" + s2
  }

  "Stage" should {
    "evict when entry is expired" in{
      var evicted = false

      val stage = newStage(None, Some(200), {(key: String, value: String) => evicted = key == "foo" && value == "bar"})
      stage.put("foo", "bar")
      Thread.sleep(400)
      stage.put("bar", "baz")

      evicted must eventually (be (true))
    }
    "evict when stage is over capacity" in{
      var evicted = false

      val stage = newStage(None, None, {(key: String, value: String) => evicted = key == "foo" && value == "bar"}, 1)
      stage.put("foo", "bar")
      stage.put("bar", "baz")

      evicted must eventually (be (true))
    }
    "evict when stage is flushed" in{
      var evicted = false

      val stage = newStage(Some(1), None, {(key: String, value: String) => evicted = key == "foo" && value == "bar"})
      stage.put("foo", "bar")
      stage.flushAll

      evicted must eventually (be (true))
    }
    "evict automatically" in{
      var evicted = false

      val stage = newStage(Some(1000), None, {(key: String, value: String) => evicted = key == "foo" && value == "bar"})
      stage.put("foo", "bar")

      Thread.sleep(1000)

      evicted must eventually (be (true))
    }
    "evict once even more then element is added" in{
      var evicted = 0

      val stage = newStage(Some(1000), None, {(key: String, value: String) => evicted = evicted + 1})
      stage.put("foo", "bar")
      stage.put("baz", "foo")
      stage.put("bar", "baz")

      Thread.sleep(1000)

      evicted must eventually (be (1))
    }
  }

  private def newStage[T](
    timeToIdle: Option[Long] = None,
    timeToLive: Option[Long] = None,
    evict:      (String, String) => Unit = {(key: String, value: String) => () },
    capacity:   Int = 100) = {

    Stage[String, String](ExpirationPolicy(timeToIdle, timeToLive, MILLISECONDS), capacity, evict)
  }
}
