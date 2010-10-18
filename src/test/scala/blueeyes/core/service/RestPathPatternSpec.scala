package blueeyes.core.service

import org.specs.Specification
import PathElement._
import RestPathPattern._

class RestPathPatternSpec extends Specification{
  "matches correct path" in {
    (RestPathPattern.Root / "foo").isDefinedAt("foo") mustEqual(true)
  }
  "matches complex correct path" in {
    ("foo" / "bar" / 'param).isDefinedAt("foo/bar/value") mustEqual(true)
  }
  "does not match incorrect path" in {
    (RestPathPattern.Root / "foo").isDefinedAt("bar") mustEqual(false)
  }
  "create parameters" in {
    (RestPathPattern.Root / 'param).apply("value") mustEqual(Map[Symbol, String]('param -> "value"))
  }
  "create parameters for complex path" in {
    ("foo" / "bar" / 'param).apply("foo/bar/value") mustEqual(Map[Symbol, String]('param -> "value"))
  }
}