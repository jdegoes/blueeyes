package blueeyes.core.service

import org.specs.Specification

class SymbolElementSpec  extends Specification{
  "matches any path" in {
    SymbolElement('foo).isDefinedAt("bar") mustEqual(true)
  }
  "create parameters" in {
    SymbolElement('foo).apply("bar") mustEqual(('foo -> "bar") :: Nil)
  }
}