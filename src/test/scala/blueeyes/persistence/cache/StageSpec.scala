package blueeyes.persistence.cache

import org.spex.Specification
import java.util.concurrent.TimeUnit.{MILLISECONDS}
import java.util.concurrent.CountDownLatch

class StageSpec extends Specification{

  "Stage.+=: coalesces entries" in{
    val stage = newStage()

    stage += (("foo", "baz"))

    stage.get("foo") must beSome("baz-bar")
  }
  "Stage.+=: adds entry" in{
    val stage = newStage()

    stage.get("foo") must beSome("bar")
  }
  "Stage.-=: removes entry" in{
    val stage = newStage()

    stage -= ("foo")

    stage.get("foo") must beNone
  }
  "Stage.iterator: returns all entries" in{
    val stage = newStage()

    stage.iterator.next must beEqual(("foo", "bar"))
  }
  "Stage.stop: evicts entries" in{
    var evicted = false

    val stage = newStage(None, None, {(key: String, value: String) => evicted = key == "foo" && value == "bar"})

    stage.stop

    evicted must be (true)
  }
  "Stage.getLater: gets value later" in{
    val stage = newStage()

    val future = stage.getLater("foo")

    val latch = new CountDownLatch(1)
    future.deliverTo({f => latch.countDown()})

    future.value  must beSome(Some("bar"))
  }
  "Stage: evicts when idle time is expired" in{
    val stage = newStage(Some(1000))

    Thread.sleep(2000)

    stage.get("foo")  must beNone
  }
  "Stage: evicts when live time is expired" in{
    val stage = newStage(None, Some(1000))

    Thread.sleep(2000)

    stage.get("foo")  must beNone
  }
  "Stage: evict is called when entry is expired" in{
    var evicted = false
    val stage = newStage(None, Some(1000), {(key: String, value: String) => evicted = key == "foo" && value == "bar"})

    Thread.sleep(2000)

    stage.get("foo")

    evicted must be (true)
  }

  private def newStage(timeToIdle: Option[Long] = None, timeToLive: Option[Long] = None, evict: (String, String) => Unit = {(key: String, value: String) => }) = {
    val stage = new Stage[String, String](settings(timeToIdle, timeToLive, evict), (key: String, v1: String, v2: String) => v1 + "-" + v2)

    stage.start

    stage += (("foo", "bar"))

    stage
  }

  private def settings(timeToIdle: Option[Long] = None, timeToLive: Option[Long] = None,
                       evict: (String, String) => Unit = {(key: String, value: String) => }) = CacheSettings[String, String](ExpirationPolicy(timeToIdle, timeToLive, MILLISECONDS), 100, evict)
}