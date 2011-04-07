package blueeyes.persistence.cache

import org.specs.Specification
import java.util.concurrent.TimeUnit.{MILLISECONDS}

class CacheSpec extends Specification{
  "Cache.concurrent: creates new map" in {
    Cache.concurrent(settings()) must not be (null)
  }
  "Cache.concurrent: adds new Entry" in {
    val map = Cache.concurrent(settings())
    map.put("foo", "bar")

    map.get("foo") must beSome("bar")
  }

  "Cache.concurrent.put: evict eldest entry" in {
    var evicted = false
    val map = Cache.concurrent(CacheSettings[String, String](ExpirationPolicy(None, None, MILLISECONDS), 1, {(key: String, value: String) => evicted = key == "foo" && value == "bar"}, 1))
    map.put("foo", "bar")
    map.put("baz", "foo")

    evicted        must be (true)
    map.get("foo") must beNone
    map.get("baz") must beSome("foo")
  }

  "Cache.concurrent.put: evicts when idle time is expired" in{
    val map = Cache.concurrent(settings(Some(1000)))
    map.put("baz", "bar")
    Thread.sleep(2000)
    map.contains("baz") must be (false)
  }
  "Cache.concurrent.put: evicts when live time is expired" in{
    val map = Cache.concurrent(settings(None, Some(1000)))
    map.put("baz", "bar")
    Thread.sleep(2000)
    map.contains("baz")  must be (false)
  }
  "Cache.concurrent: evict is called when entry is expired" in{
    var expired = false
    val map = Cache.concurrent(settings(None, Some(1000), {(key: String, value: String) => expired = key == "foo" && value == "bar"}))
    map.put("foo", "bar")
    Thread.sleep(2000)
    map.contains("foo")

    expired must be (true)
  }
  "Cache.concurrent: adds new Entry" in {
    val map = Cache.concurrent(settings())
    map.put("foo", "bar")

    map.get("foo") must beSome("bar")
  }

  "Cache.concurrent.put: evicts when idle time is expired" in{
    val map = Cache.concurrent(settings(Some(1000)))
    map.put("baz", "bar")
    Thread.sleep(2000)
    map.contains("baz") must be (false)
  }
  "Cache.concurrent.put: evicts when live time is expired" in{
    val map = Cache.concurrent(settings(None, Some(1000)))
    map.put("baz", "bar")
    Thread.sleep(2000)
    map.contains("baz")  must be (false)
  }
  "Cache.concurrent: evict is called when entry is expired" in{
    var expired = false
    val map = Cache.concurrent(settings(None, Some(1000), {(key: String, value: String) => expired = key == "foo" && value == "bar"}))
    map.put("foo", "bar")
    Thread.sleep(2000)
    map.contains("foo")

    expired must be (true)
  }


  private def settings(timeToIdle: Option[Long] = None, timeToLive: Option[Long] = None,
                       evict: (String, String) => Unit = {(key: String, value: String) => }) = CacheSettings[String, String](ExpirationPolicy(timeToIdle, timeToLive, MILLISECONDS), 100, evict)
}