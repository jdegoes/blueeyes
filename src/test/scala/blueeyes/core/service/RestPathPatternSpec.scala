package blueeyes.core.service

import org.specs.Specification

class RestPathPatternSpec extends Specification{
  import RestPathPattern2Implicits._
  
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
  "match root path" in {
    testPath("/",
      List(("/", Map())),
      List("")
    )
  }
  "combine symbols and literals using slash operator" in {
    val pattern: RestPathPattern2 = "/foo" / 'bar / 'biz / "blah"
    
    pattern("/foo/a/b/blah") mustEqual(Map('bar -> "a", 'biz -> "b"))
  }
  "match manually formed path including root literal" in {
    (RestPathPattern2.Root / "foo").isDefinedAt("/foo") mustEqual(true)
  }
  "match complex path with symbols" in {
    (RestPathPattern2.Root / "foo" / "bar" / 'param).isDefinedAt("/foo/bar/value") mustEqual(true)
  }
  "create single parameter" in {
    (RestPathPattern2.Root / 'param).apply("/value") mustEqual(Map[Symbol, String]('param -> "value"))
  }
  "create multiple parameters" in {
    (RestPathPattern2.Root / 'param1 / 'param2).apply("/value1/value2") mustEqual(Map[Symbol, String]('param1 -> "value1", 'param2 -> "value2"))
  }
  "create single parameter in lengthy literal path" in {
    (RestPathPattern2.Root / "foo" / "bar" / 'param).apply("/foo/bar/value") mustEqual(Map[Symbol, String]('param -> "value"))
  }
  "create parameters automatically for complex path specified as string" in {
    val pattern: RestPathPattern2 = "/foo/bar/'param"
    
    pattern.apply("/foo/bar/value") mustEqual(Map[Symbol, String]('param -> "value"))
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