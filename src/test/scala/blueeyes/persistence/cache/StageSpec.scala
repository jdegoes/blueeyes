package blueeyes.persistence.cache

import org.specs.Specification
import java.util.concurrent.TimeUnit.{MILLISECONDS}
import java.util.concurrent.CountDownLatch

import scalaz.Semigroup

class StageSpec extends Specification{
  implicit val StringSemigroup = new Semigroup[String] {
    def append(s1: String, s2: => String) = s1 + "-" + s2
  }

  "Stage" should {
    "evict when entry is expired" in{
      var evicted = false

      newStage(None, Some(1000), {(key: String, value: String) => evicted = key == "foo" && value == "bar"}) { stage =>
        Thread.sleep(2000)

        evicted must be (true)
      }
    }
  }

  private def newStage[T](
    timeToIdle: Option[Long] = None,
    timeToLive: Option[Long] = None,
    evict:      (String, String) => Unit = {(key: String, value: String) => () },
    capacity:   Int = 100)
    (f: Stage[String, String] => T): T = {

    val stage = Stage[String, String](ExpirationPolicy(timeToIdle, timeToLive, MILLISECONDS), capacity, evict)

    stage += ("foo", "bar")

    try {
      f(stage)
    }
    catch {
      case e: Throwable => e.printStackTrace; throw e;
    }
  }
}
