package blueeyes.parsers

import org.specs.{Specification, ScalaCheck}
import util.parsing.input.CharSequenceReader
import org.scalacheck.Gen
import org.scalacheck.Prop.forAllNoShrink
import W3ExtendedLogAST._
import W3ExtendedLogGen._

class W3ExtendedLogGrammarSpec extends Specification with ScalaCheck{
  implicit def stringToInput(s: String) = new CharSequenceReader(s)

  "W3ExtendedLogGrammar: parses Version" in {
    passTest(versionDirective)
  }

  "W3ExtendedLogGrammar: parses Software" in {
    passTest(softwareDirective)
  }

  "W3ExtendedLogGrammar: parses Start-Date" in {
    passTest(startDateDirective)
  }
  "W3ExtendedLogGrammar: parses End-Date" in {
    passTest(endDateDirective)
  }
  "W3ExtendedLogGrammar: parses Date" in {
    passTest(dateDirective)
  }

  "W3ExtendedLogGrammar: parses Remark" in {
    passTest(remarkDirective)
  }

  "W3ExtendedLogGrammar: parses fieldsDirective" in {
    passTest(fieldsDirective)
  }

  "W3ExtendedLogGrammar: parses directives" in {
    passTest(directives, "\n")
  }

  private def passTest(gen: Gen[String], delim: String = "") = forAllNoShrink(gen)(n => >>(W3ExtendedLog(n), delim) == n) must pass

  def >> (directives : List[Directive], delim: String) = directives.map(_.toString).mkString(delim)
}