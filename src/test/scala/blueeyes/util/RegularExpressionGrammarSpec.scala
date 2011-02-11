package blueeyes.util

import org.spex.Specification
import org.specs.ScalaCheck
import org.scalacheck._
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.forAllNoShrink
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import blueeyes.util.RegularExpressionAST.RegexAtom
import RegexpGen._

class RegularExpressionGrammarSpec extends Specification with ScalaCheck{

  implicit def stringToInput(s: String) = new CharSequenceReader(s)

  "RegularExpressionGrammar: parses Regex Atom" in {
    passTest(regexAtom)
  }
  
  "RegularExpressionGrammar: parses single fixed chars" in{
    passTest(singleFixedChar)
  }
  
  "RegularExpressionGrammar: parses digit number" in{
    passTest(digit)
  }
  
  "RegularExpressionGrammar: parses small num hex" in{
    passTestCaseInsensitive(smallHexNumber)
  }

  "RegularExpressionGrammar: parses unicode char" in{
    passTestCaseInsensitive(unicodeChar)
  }

  "RegularExpressionGrammar: parses octal char" in{
    passTest(octalNumber)
  }

  "RegularExpressionGrammar: parses other char" in{
    passTest(otherChar)
  }

  "RegularExpressionGrammar: parses boundary match" in{
    passTest(boundaryMatch)
  }

  "RegularExpressionGrammar: parses Shorthand Character Class" in{
    passTest(shorthandCharacterClass)
  }

  "RegularExpressionGrammar: parses Posix Character Class" in{
    passTest(posixCharacterClass)
  }

  "RegularExpressionGrammar: parses Escape Sequence" in{
    passTest(escapeSequence)
  }

  "RegularExpressionGrammar: parses Flag Group" in{
    passTest(flags)
    passTest(flagGroup)
  }

  "RegularExpressionGrammar: parses Quotation" in{
    passTest(quotation)
  }
  "RegularExpressionGrammar: parses Back Reference" in{
    passTest(backReference)
  }

  "RegularExpressionGrammar: parses Single Char " in{
    passTest(singleChar)
  }

  "RegularExpressionGrammar: parses Character Class" in{
    passTest(characterClass)
  }

  "RegularExpressionGrammar: parses Non Capturing Group" in{
    passTest(nonCapturingGroup)
  }

  "RegularExpressionGrammar: parses Atomic Group" in{
    passTest(atomicGroup)
  }

  "RegularExpressionGrammar: parses Named Capture Group" in{
    passTest(namedCaptureGroup)
  }

  "RegularExpressionGrammar: parses Look Around" in{
    passTest(lookAround)
  }

  "RegularExpressionGrammar: parses Group" in{
    passTest(group)
  }

  private def passTest(gen: Gen[String]) = forAllNoShrink(gen)(n => >>(RegularExpressionPatten(n)) == n) must pass
  private def passTestCaseInsensitive(gen: Gen[String]) = forAllNoShrink(gen)(n => >>(RegularExpressionPatten(n)).toLowerCase == n.toLowerCase) must pass

  def >> (regexp : List[RegexAtom]): String = regexp.map(_.toString).mkString("")
}
