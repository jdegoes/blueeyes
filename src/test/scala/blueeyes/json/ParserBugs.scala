package blueeyes.json

import org.specs.Specification
import org.specs.runner.{Runner, JUnit}
import scala.util.control.Exception._

object ParserBugs extends Specification {
  "Unicode ffff is a valid char in string literal" in {
    JsonParser.parseOpt(""" {"x":"\uffff"} """).isDefined mustEqual true
  }

  "Dos not hang when parsing 2.2250738585072012e-308" in {
    allCatch.opt(JsonParser.parse(""" [ 2.2250738585072012e-308 ] """)) mustEqual None
    allCatch.opt(JsonParser.parse(""" [ 22.250738585072012e-309 ] """)) mustEqual None
  }
}