package blueeyes.persistence.cache

import org.spex.Specification

class LRUMapSpec extends Specification{
  "LRUMap.put: adds new value" in {
    val map = newMap()
    map.put("foo", "bar")
    map.get("foo") mustEqual("bar")
  }
  "LRUMap.put: evict eldest entry" in {
    var evicted = false
    val map = newMap((key: String, value: String) => evicted = key == "foo" && value == "bar")
    map.put("foo", "bar")
    map.put("baz", "foo")

    evicted        must be (true)
    map.get("foo") must be  (null)
    map.get("baz") mustEqual("foo")
  }
  "LRUMap.remove: removes entry and decrement Total Weight" in {
    val map = newMap()
    map.put("foo", "bar")
    map.remove("foo") mustEqual ("bar")
    map.put("baz", "foo")

    map.get("foo") must be  (null)
    map.get("baz") mustEqual("foo")
  }
  "LRUMap.remove(key, value): removes entry and decrement Total Weight" in {
    val map = newMap()
    map.put("foo", "bar")
    map.remove("foo", "bar") mustEqual (true)
    map.put("baz", "foo")

    map.get("foo") must be  (null)
    map.get("baz") mustEqual("foo")
  }
  "LRUMap.clear: clears map and decrement Total Weight" in {
    val map = newMap()
    map.put("foo", "bar")
    map.clear()
    map.put("baz", "foo")

    map.get("foo") must be  (null)
    map.get("baz") mustEqual("foo")
  }
  "LRUMap.putIfAbsent: does not add existing key" in {
    val map = newMap()
    map.putIfAbsent("foo", "bar")
    map.putIfAbsent("foo", "baz")

    map.get("foo") must be  ("bar")
  }
  "LRUMap.replace: replaces existing entry" in {
    val map = newMap()
    map.put("foo", "bar")
    map.replace("foo", "baz")

    map.get("foo") must be  ("baz")
  }
  "LRUMap.replace(key, oldValue, value): replaces existing entry" in {
    val map = newMap()
    map.put("foo", "bar")
    map.replace("foo", "bar", "baz")

    map.get("foo") must be  ("baz")
  }
  "LRUMap.replace: does not replace not existing entry" in {
    val map = newMap()
    map.put("foo", "bar")
    map.replace("baz", "foo")

    map.get("baz") must be  (null)
  }
  private def newMap(evicter: (String, String) => Unit = {(key: String, value: String) => ()}) = new LRUMap[String, String](evicter, 1, {value: String => 1})
}