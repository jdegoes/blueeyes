package blueeyes.parsers

import util.parsing.input.CharSequenceReader
import org.scalacheck.Gen
import org.scalacheck.Prop.forAllNoShrink
import W3ExtendedLogAST._
import W3ExtendedLogGen._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

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

  private def passTest(gen: Gen[String], delim: String = "") = forAllNoShrink(gen)(n => >>(W3ExtendedLog(n), delim) must_== n)

  def >> (directives : List[Directive], delim: String) = directives.map(_.toString).mkString(delim)
}