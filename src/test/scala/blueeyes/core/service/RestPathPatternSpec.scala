package blueeyes.core.service

import org.specs.Specification
import RestPathPatternImplicits._

class RestPathPatternSpec extends Specification{
  "match correct literal path containing a single path element" in {
    testPath("/foo",
      List(("/foo", Map())),
      List("/bar")
    )
  }
  "match correct literal path containing many path elements" in {
    testPath("/foo/bar/baz",
      List(("/foo/bar/baz", Map())),
      List("/bar/baz/bar")
    )
  }
  "match correct literal and symbol path containing many path elements" in {
    testPath("/foo/'bar/baz",
      List(("/foo/baz/baz", Map('bar -> "baz")), ("/foo/boo/baz", Map('bar -> "boo")), ("/foo/foo/baz", Map('bar -> "foo"))),
      List("/bar/bar/bar")
    )
  }
  "combine symbols and literals using slash operator" in {
    import RestPathPattern2Implicits._
    
    ("/foo" / 'bar / 'biz / "blah").apply("/foo/a/b/blah") mustEqual(Map('bar -> "a", 'biz -> "b"))
  }
  
  
  "matches correct path started with slash" in {
    (RestPathPattern.Root / StringElement("foo")).isDefinedAt("/foo") mustEqual(true)
  }
  "matches complex correct path" in {
    (RestPathPattern.Root / StringElement("foo") / StringElement("bar") / 'param).isDefinedAt("foo/bar/value") mustEqual(true)
  }
  "does not match incorrect path" in {
    (RestPathPattern.Root / StringElement("foo")).isDefinedAt("bar") mustEqual(false)
  }
  "create parameters" in {
    (RestPathPattern.Root / SymbolElement('param)).apply("value") mustEqual(Map[Symbol, String]('param -> "value"))
  }
  "create parameters when path started with slash" in {
    (RestPathPattern.Root / SymbolElement('param)).apply("/value") mustEqual(Map[Symbol, String]('param -> "value"))
  }
  "create parameters for complex path" in {
    (RestPathPattern.Root / StringElement("foo") / StringElement("bar") / 'param).apply("foo/bar/value") mustEqual(Map[Symbol, String]('param -> "value"))
  }
  "create parameters automatically for complex path specified as string" in {
    val pattern: RestPathPattern = "/foo/bar/'param"
    
    pattern.apply("foo/bar/value") mustEqual(Map[Symbol, String]('param -> "value"))
  }
  "create parameters automatically for root path specified as string" in {
    val pattern: RestPathPattern = "/"
    
    pattern.isDefinedAt("") mustEqual(true)
  }
  
  private def testPath(path: String, isDefinedAt: List[(String, Map[Symbol, String])], isNotDefinedAt: List[String]) {
    val pattern = RestPathPattern2(path)
    
    isDefinedAt.foreach { pair =>
      val path = pair._1
      val map  = pair._2
      
      pattern.isDefinedAt(path) mustEqual(true)
      pattern.apply(path) mustEqual(map)
    }
    
    isNotDefinedAt.foreach { path =>
      pattern.isDefinedAt(path) mustEqual(false)
    }
  }
}