package blueeyes.core.service

import org.specs.Specification

class IdentifierWithDefaultSpec extends Specification{
  "IdentifierWithDefault.default: lazy loads default value" in {
    val identifierWithDefault = IdentifierWithDefault[String, String]("foo", () => sys.error("lazy"))

    identifierWithDefault.default must throwAn[java.lang.RuntimeException]
  }
}