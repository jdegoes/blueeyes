package blueeyes.persistence.cache

import org.spex.Specification
import java.util.concurrent.TimeUnit.{MILLISECONDS}
import java.util.concurrent.CountDownLatch

class StageSpec extends Specification{
  "Stage += " should {
    "coalesces entries with duplicate keys" in {
      newStage() { stage =>
        stage += (("foo", "baz"))

        stage.get("foo") must eventually (beSome("bar-baz"))
      }
    }
    
    "add entry" in {
      newStage() { stage =>
        stage.get("foo") must eventually (beSome("bar"))
      }
    }
  }
  
  "Stage -= " should {
    "remove entry" in {
      newStage() { stage =>
        stage -= ("foo")

        stage.get("foo") must eventually (beNone)
      }
    }
  }
  
  "Stage.iterator" should {
    "returns all entries" in {
      newStage() { stage =>
        stage.iterator.hasNext must eventually (beEqual(true))
      }
    }
  }
  
  "Stage.stop" should {
    "evict all entries" in {
      @volatile var _evicted = false
      
      newStage(None, None, {(key: String, value: String) => _evicted = true}) { stage =>
        stage.stop

        _evicted must eventually (beEqual(true))
      }
    }
  }

  "Stage.getLater" should {
    "return a future of the value" in {
      newStage() { stage =>
        val future = stage.getLater("foo")

        val latch = new CountDownLatch(1)
        future.deliverTo({f => latch.countDown()})

        latch.await

        future.value must beSome(Some("bar"))
      }
    }
  }

  "Failing Stage.getLater" should {
    "return a future of the value" in {
      newStage() { stage =>
        stage += (("foo", "bar"))
        val future = stage.getLater("foo")
        future.value must eventually (beSome(Some("bar")))
      }
    }
  }

  "Another Failing Stage.getLater" should {
    "return a future of the value" in {
      newStage() { stage =>
        stage += (("fiz", "biz"))
        val future = stage.getLater("fiz")
        future.value must eventually (beSome(Some("biz")))
      }
    }
  }
  
  "Stage" should {
    "evict when idle time is expired" in {
      newStage(Some(1000)) { stage =>
        Thread.sleep(2000)

        stage.get("foo")  must beNone
      }
    }
    "evict when live time is expired" in{
      newStage(None, Some(1000)) { stage =>
        Thread.sleep(2000)

        stage.get("foo")  must beNone
      }
    }
    "evict when entry is expired" in{
      var evicted = false
      
      newStage(None, Some(1000), {(key: String, value: String) => evicted = key == "foo" && value == "bar"}) { stage =>
        Thread.sleep(2000)

        stage.get("foo")

        evicted must be (true)
      }
    }
  }

  private def newStage[T](timeToIdle: Option[Long] = None, timeToLive: Option[Long] = None, evict: (String, String) => Unit = {(key: String, value: String) => })(f: Stage[String, String] => T): T = {
    val stage = new Stage[String, String](settings(timeToIdle, timeToLive, evict), (key: String, v1: String, v2: String) => v1 + "-" + v2)

    stage += (("foo", "bar"))

    try f(stage)
    finally stage.stop
  }

  private def settings(timeToIdle: Option[Long] = None, timeToLive: Option[Long] = None,
                       evict: (String, String) => Unit = {(key: String, value: String) => }) = CacheSettings[String, String](ExpirationPolicy(timeToIdle, timeToLive, MILLISECONDS), 100, evict)
}
