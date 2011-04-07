package blueeyes.parsers

import org.specs.Specification
import org.specs.ScalaCheck
import org.scalacheck._
import org.scalacheck.Prop.forAllNoShrink
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import RegularExpressionAST.RegexAtom
import RegexpGen._

class RegularExpressionGrammarSpec extends Specification with ScalaCheck{

  implicit def stringToInput(s: String) = new CharSequenceReader(s)

  "RegularExpressionGrammar" should {
    "parse Regex Atom" in {
      passTest(regexAtom)
    }

    "parse single fixed chars" in{
      passTest(singleFixedChar)
    }

    "parse digit number" in{
      passTest(digit)
    }

    "parse small num hex" in{
      passTestCaseInsensitive(smallHexNumber)
    }

    "parse unicode char" in{
      passTestCaseInsensitive(unicodeChar)
    }

    "parse octal char" in{
      passTest(octalNumber)
    }

    "parse other char" in{
      passTest(otherChar)
    }

    "parse boundary match" in{
      passTest(boundaryMatch)
    }

    "parse Shorthand Character Class" in{
      passTest(shorthandCharacterClass)
    }

    "parse Posix Character Class" in{
      passTest(posixCharacterClass)
    }

    "parse Escape Sequence" in{
      passTest(escapeSequence)
    }

    "parse Flag Group" in{
      passTest(flags)
      passTest(flagGroup)
    }

    "parse Quotation" in{
      passTest(quotation)
    }
    "parse Back Reference" in{
      passTest(backReference)
    }

    "parse Single Char " in{
      passTest(singleChar)
    }

    "parse Character Class" in{
      passTest(characterClass)
    }

    "parse Non Capturing Group" in{
      passTest(nonCapturingGroup)
    }

    "parse Atomic Group" in{
      passTest(atomicGroup)
    }

    "parse Named Capture Group" in{
      passTest(namedCaptureGroup)
    }

    "parse Look Around" in{
      passTest(lookAround)
    }

    "parse Group" in{
      passTest(group)
    }

    "Regression 1" in {
      RegularExpressionPatten.isDefinedAt("""(?<prefixPath>(?:[^\n.](?:[^\n/]|/[^\n\.])+)/?)?""") must beTrue
    }
  }

  private def passTest(gen: Gen[String]) = forAllNoShrink(gen)(n => >>(RegularExpressionPatten(n)) == n) must pass
  private def passTestCaseInsensitive(gen: Gen[String]) = forAllNoShrink(gen)(n => >>(RegularExpressionPatten(n)).toLowerCase == n.toLowerCase) must pass

  def >> (regexp : List[RegexAtom]): String = regexp.map(_.toString).mkString("")
}
