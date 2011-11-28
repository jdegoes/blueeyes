package blueeyes.json

import org.specs2.mutable.Specification
import scala.util.control.Exception._
import java.net.URLDecoder
import JsonAST._

object ParserBugs extends Specification {
  "Unicode ffff is a valid char in string literal" in {
    JsonParser.parseOpt(""" {"x":"\uffff"} """).isDefined mustEqual true
  }

  "Does not hang when parsing 2.2250738585072012e-308" in {
    allCatch.opt(JsonParser.parse(""" [ 2.2250738585072012e-308 ] """)) mustEqual None
  }

  "Does not hang when parsing 22.250738585072012e-309" in {
    allCatch.opt(JsonParser.parse(""" [ 22.250738585072012e-309 ] """)) mustEqual None
  }

  "Can parse funky characters" in {
    JsonParser.parse(URLDecoder.decode("\"%E2%84%A2\"", "UTF-8")) must_== JString("™")
  }
}
