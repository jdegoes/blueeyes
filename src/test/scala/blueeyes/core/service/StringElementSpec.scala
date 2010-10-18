package blueeyes.core.service

import org.specs.Specification

class StringElementSpec  extends Specification{
  "matches correct path" in {
    StringElement("foo").isDefinedAt("foo") mustEqual(true)
  }
  "does not match incorrect path" in {
    StringElement("foo").isDefinedAt("bar") mustEqual(false)
  }
  "does not create parameters" in {
    StringElement("foo").apply("bar") mustEqual(Map())
  }
}