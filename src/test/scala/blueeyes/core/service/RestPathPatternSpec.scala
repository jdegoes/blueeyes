package blueeyes.core.service

import org.specs.Specification
import RestPathPatternImplicits._

class RestPathPatternSpec extends Specification{
  "matches correct path" in {
    (RestPathPattern.Root / StringElement("foo")).isDefinedAt("foo") mustEqual(true)
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
  "create parameters for complex path" in {
    (RestPathPattern.Root / StringElement("foo") / StringElement("bar") / 'param).apply("foo/bar/value") mustEqual(Map[Symbol, String]('param -> "value"))
  }
  "create parameters automatically for complex path specified as string" in {
    val pattern: RestPathPattern = "/foo/bar/'param"
    
    pattern.apply("foo/bar/value") mustEqual(Map[Symbol, String]('param -> "value"))
  }
}