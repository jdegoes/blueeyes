package blueeyes.util

import scala.util.parsing.combinator._
import scala.util.matching.Regex

object RegularExpressionAST {
  case class RegularExpression(atoms: RegexAtom*)

  case class RegexAtom(element: RegexAtomElement, quantifier: Option[Quantifier])
  sealed trait RegexAtomElement

  sealed trait Quantifier
  case class GreedyQuantifier(lowerBound: Int, upperBound: Option[Int]) extends Quantifier
  case class ReluctantQuantifier(quantifier: GreedyQuantifier) extends Quantifier
  case class PossessiveQuantifier(quantifier: GreedyQuantifier) extends Quantifier

  abstract class SingleChar extends RegexAtomElement { def char: Char } 
  abstract class FixedSingleChar(val char: Char) extends SingleChar
  case object AndChar extends FixedSingleChar('&')
  case object HyphenChar extends FixedSingleChar('-')
  case object ColonChar extends FixedSingleChar(':')
  case object SquareBracketStartChar extends FixedSingleChar('[')
  case object SquareBracketEndChar extends FixedSingleChar(']')
  case object CurlyBracketStartChar extends FixedSingleChar('}')
  case object CurlyBracketEndChar extends FixedSingleChar('}')
  case object EqualsChar extends FixedSingleChar('=')
  case object LessThanChar extends FixedSingleChar('<')
  case object GreaterThanChar extends FixedSingleChar('>')
  case object ExclamationMarkChar extends FixedSingleChar('!')
  case object CommaChar extends FixedSingleChar(',')
  case class DigitChar(char: Char) extends SingleChar
  case class SmallHexNumber(char: Char) extends SingleChar
  case class UnicodeChar(char: Char) extends SingleChar
  case class OctalNumber(char: Char) extends SingleChar
  case class OtherChar(char: Char) extends SingleChar
  
  sealed trait BoundaryMatch extends RegexAtomElement
  case object Caret extends BoundaryMatch
  case object Dollar extends BoundaryMatch
  case class BoundaryMatchEscape(escape: Char) extends BoundaryMatch

  sealed trait ShorthandCharacterClass extends RegexAtomElement
  case object DigitClass    extends ShorthandCharacterClass
  case object NotDigitClass extends ShorthandCharacterClass
  case object SpaceClass    extends ShorthandCharacterClass
  case object NotSpaceClass extends ShorthandCharacterClass
  case object WordClass     extends ShorthandCharacterClass
  case object NotWordClass  extends ShorthandCharacterClass
  
  sealed trait PosixCharacterClass extends RegexAtomElement
  case object PosixLower extends PosixCharacterClass
  case object PosixUpper extends PosixCharacterClass
  case object PosixASCII extends PosixCharacterClass
  case object PosixAlpha extends PosixCharacterClass
  case object PosixDigitChar extends PosixCharacterClass
  case object PosixAlnum extends PosixCharacterClass
  case object PosixPunct extends PosixCharacterClass
  case object PosixGraph extends PosixCharacterClass
  case object PosixPrint extends PosixCharacterClass
  case object PosixBlank extends PosixCharacterClass
  case object PosixCntrl extends PosixCharacterClass
  case object PosixXDigit extends PosixCharacterClass
  case object PosixSpace extends PosixCharacterClass
  
  case class EscapeSequence() extends RegexAtomElement
  case class CharacterClass() extends RegexAtomElement
  case class FlagGroup(flags1: List[Char], flags2: Option[List[Char]]) extends RegexAtomElement
  case class Quotation() extends RegexAtomElement
  case class NamedCaptureGroup(name: String, group: RegexAtom*) extends RegexAtomElement
  case class NonCapturingGroup(flags: Option[(List[Char], Option[List[Char]])], group: RegexAtom*) extends RegexAtomElement
  case class AtomicGroup(group: RegexAtom*) extends RegexAtomElement
  case class LookAround(isLookbehind: Boolean, isNegative: Boolean, regex: RegexAtom*) extends RegexAtomElement
  case class Group(group: RegexAtom*) extends RegexAtomElement
  case class BackReference(reference: Int) extends RegexAtomElement
  case class DotChar() extends RegexAtomElement
  case class OrChar() extends RegexAtomElement
  
}

trait RegularExpressionGrammar extends RegexParsers {
  import RegularExpressionAST._

  private[this] implicit def string2Regex(s: String): Regex = new Regex(s)
  
  def regularExpression = startOfFile ~ (regexAtom*) ~ endOfFile

  def regexAtom: Parser[RegexAtom] = (
    singleChar |
    boundaryMatch | 
    shorthandCharacterClass |
    posixCharacterClass |
    escapeSequence |
    characterClass |
    flagGroup |
    quotation |
    namedCaptureGroup |
    nonCapturingGroup |
    atomicGroup |
    lookAround |
    group |
    backReference |
    dotChar |
    orChar
  ) ~ (quantifier?) ^^ {case element ~ quant => RegexAtom(element, quant)}

  def backReference: Parser[RegexAtomElement] = escapeChar ~> (decimal) ^^ {value => BackReference(value)}

  def singleChar: Parser[RegexAtomElement] = (andChar | 
    hyphenChar |
    colonChar |
    squareBracketEndChar |
    curlyBracketEndChar |
    equalsChar |
    lessThanChar |
    greaterThanChar |
    exclamationMarkChar |
    commaChar |
    digitChar |
    smallHexNumber |
    unicodeChar |
    octalNumber |
    otherChar
  )

  def atomicGroup: Parser[AtomicGroup] = roundBracketStartChar ~> questionMarkChar ~> greaterThanChar ~> (regexAtom*) <~ roundBracketEndChar ^^ {value => AtomicGroup(value: _*)} 

  def flagGroup: Parser[FlagGroup] = roundBracketStartChar ~> questionMarkChar ~> flags <~ roundBracketEndChar ^^ (value => FlagGroup(value._1, value._2))

  def flags = (otherChar*) ~ ((hyphenChar ~> (otherChar*))?) ^^ {case flags1 ~ flags2 => Tuple2(flags1.map(_.char), flags2.map(_.map(_.char)))}

  def lookAround: Parser[LookAround] = roundBracketStartChar ~> questionMarkChar ~> (lessThanChar?) ~ (equalsChar | exclamationMarkChar) ~ (regexAtom*) <~ roundBracketEndChar ^^ {case lookbehind ~ negative ~ value => LookAround(lookbehind.map(v => true).getOrElse(false), negative == ExclamationMarkChar, value: _*)}
  
  def group: Parser[Group] = roundBracketStartChar ~> (regexAtom*) <~ roundBracketEndChar ^^ {value => Group(value: _*)}
  
  def namedCaptureGroup: Parser[NamedCaptureGroup] = roundBracketStartChar ~> questionMarkChar ~> (lessThanChar ~> (regex("[a-zA-z0-9]")+) <~ greaterThanChar) ~ (regexAtom*) <~ roundBracketEndChar  ^^ {case name ~ value => NamedCaptureGroup(name.mkString(""), value: _*)}

  def nonCapturingGroup: Parser[NonCapturingGroup] = roundBracketStartChar ~> questionMarkChar ~> (flags?) ~ (colonChar ~> (regexAtom*)) <~ roundBracketEndChar ^^ {case flags ~ value => NonCapturingGroup(flags, value: _*)}

  def characterClass: Parser[CharacterClass] = squareBracketStartChar ~ (caretChar?) ~ (squareBracketEndChar ~ (characterClassAtom*) | (characterClassAtom+)) ~ squareBracketEndChar ^^^ CharacterClass()

  def characterClassAtom = characterClassRange |
    escapeSequence |
    shorthandCharacterClass |
    posixCharacterClass |
    quotation |
    octalNumber |
    andChar |
    orChar |
    caretChar |
    colonChar |
    dollarChar |
    roundBracketStartChar |
    roundBracketEndChar |
    curlyBracketStartChar |
    curlyBracketEndChar |
    equalsChar |
    lessThanChar |
    greaterThanChar |
    exclamationMarkChar |
    commaChar |
    plusChar |
    starChar |
    questionMarkChar |
    dotChar |
    digitChar |
    hyphenChar |
    otherChar |
    smallHexNumber |
    unicodeChar

  def characterClassRange = rangeAtom ~ hyphenChar ~ rangeAtom

  def rangeAtom = smallHexNumber |
    unicodeChar |
    octalNumber |
    andChar |
    orChar |
    caretChar |
    hyphenChar |
    colonChar |
    dollarChar |
    roundBracketStartChar |
    roundBracketEndChar |
    curlyBracketStartChar |
    curlyBracketEndChar |
    equalsChar |
    lessThanChar |
    greaterThanChar |
    exclamationMarkChar |
    commaChar |
    plusChar |
    starChar |
    questionMarkChar |
    dotChar |
    digitChar |
    otherChar

  def quantifier: Parser[Quantifier] = reluctantQuantifier | possessiveQuantifier | greedyQuantifier

  def greedyQuantifier: Parser[GreedyQuantifier]  = questionMarkChar ^^^ GreedyQuantifier(0, Some(1))  | starChar ^^^ GreedyQuantifier(0, None) | plusChar ^^^ GreedyQuantifier(1, None) |
    (curlyBracketStartChar ~> (decimal ^^ {value => GreedyQuantifier(value, Some(value))} | (decimal <~ commaChar) ^^ {value => GreedyQuantifier(value, None)} | (decimal ~ commaChar ~ decimal) ^^ {case lower ~ comma ~ upper => GreedyQuantifier(lower, Some(upper))}) <~ curlyBracketEndChar)

  def reluctantQuantifier: Parser[ReluctantQuantifier] = greedyQuantifier <~ questionMarkChar ^^ (value => ReluctantQuantifier(value))

  def possessiveQuantifier: Parser[PossessiveQuantifier] = greedyQuantifier <~ plusChar ^^ (value => PossessiveQuantifier(value))

  def decimal: Parser[Int] = (digitChar+) ^^ (digitChars => digitChars.map(_.char).mkString("").toInt)

  def boundaryMatch: Parser[RegexAtomElement] = caretChar ^^^ Caret | dollarChar ^^^ Dollar | boundaryMatchEscape

  def quotation: Parser[RegexAtomElement] = quotationStart ~ ((quotationStart | 
    posixCharacterClass |
    shorthandCharacterClass |
    boundaryMatch |
    octalNumber |
    smallHexNumber |
    unicodeChar |
    escapeSequence |
    escapeChar |
    andChar |
    orChar |
    hyphenChar |
    caretChar |
    colonChar |
    dollarChar |
    squareBracketStartChar |
    squareBracketEndChar |
    roundBracketStartChar |
    roundBracketEndChar |
    curlyBracketStartChar |
    curlyBracketEndChar |
    equalsChar |
    lessThanChar |
    greaterThanChar |
    exclamationMarkChar |
    commaChar |
    plusChar |
    starChar |
    questionMarkChar |
    dotChar |
    digitChar |
    otherChar
  )*) ~ quotationEnd ^^^ Quotation()

  def quotationStart = "\\q"

  def quotationEnd = "\\e"

  def posixCharacterClass = "\\p{" ~> ("lower" ^^^ PosixLower |
    "upper" ^^^ PosixUpper |
    "aSCII" ^^^ PosixASCII |
    "alpha" ^^^ PosixAlpha |
    "digitChar" ^^^ PosixDigitChar |
    "alnum" ^^^ PosixAlnum |
    "punct" ^^^ PosixPunct |
    "graph" ^^^ PosixGraph |
    "print" ^^^ PosixPrint |
    "blank" ^^^ PosixBlank |
    "cntrl" ^^^ PosixCntrl |
    "xDigit" ^^^ PosixXDigit |
    "space" ^^^ PosixSpace
  ) <~ "}"

  def shorthandCharacterClass: Parser[RegexAtomElement] = escapeChar ~> ("d" ^^^ DigitClass |
    "D" ^^^ NotDigitClass |
    "s" ^^^ SpaceClass |
    "S" ^^^ NotSpaceClass |
    "w" ^^^ WordClass |
    "W" ^^^ NotWordClass
  )

  def boundaryMatchEscape: Parser[RegexAtomElement] = escapeChar ~> regex("[bBAzZG]") ^^ {char => BoundaryMatchEscape(char.charAt(0))}

  def octalNumber: Parser[RegexAtomElement] = escapeChar ~ "0" ~> (
    ((regex("[0-7]")?) ~ regex("[0-7]") ^^ {case v1~v2 => parseOctal(v1.getOrElse("") + v2)}) |
    (regex("[0-3]") ~ regex("[0-7]") ~ regex("[0-7]") ^^ {case v1~v2~v3 => parseOctal(v1 + v2 + v3)})
  )

  private def parseOctal(v: String) = OctalNumber(Integer.parseInt(v, 8).toChar)

  def smallHexNumber: Parser[RegexAtomElement] = escapeChar ~> "x" ~> regex("[0-9a-fA-f]{2}") ^^ {v => SmallHexNumber(Integer.parseInt(v, 16).toChar)}

  def unicodeChar: Parser[RegexAtomElement] = escapeChar ~> "u" ~> regex("[0-9a-fA-f]{4}") ^^ {v => UnicodeChar(Integer.parseInt(v, 16).toChar)}

  def escapeSequence: Parser[RegexAtomElement] = escapeChar ~ regex("[tnrfae&&[^a-zA-z0-9]]") ^^^ EscapeSequence()

  def escapeChar: Parser[String] = "\\"

  def andChar: Parser[RegexAtomElement] = "&" ^^^ AndChar

  def orChar: Parser[RegexAtomElement] = "|" ^^^ OrChar()

  def hyphenChar: Parser[RegexAtomElement] = "-" ^^^ HyphenChar

  def caretChar: Parser[String] = "^"

  def colonChar: Parser[RegexAtomElement] = ":" ^^^ ColonChar

  def dollarChar: Parser[String] = "$"

  def squareBracketStartChar: Parser[RegexAtomElement] = "[" ^^^ SquareBracketStartChar

  def squareBracketEndChar: Parser[RegexAtomElement] = "]" ^^^ SquareBracketEndChar

  def roundBracketStartChar: Parser[String] = "("

  def roundBracketEndChar: Parser[String] = ")"

  def curlyBracketStartChar: Parser[RegexAtomElement] = "{" ^^^ CurlyBracketStartChar

  def curlyBracketEndChar: Parser[RegexAtomElement] = "}" ^^^ CurlyBracketEndChar

  def equalsChar: Parser[RegexAtomElement] = "=" ^^^ EqualsChar

  def lessThanChar: Parser[RegexAtomElement] = "<" ^^^ LessThanChar

  def greaterThanChar: Parser[RegexAtomElement] = ">" ^^^ GreaterThanChar

  def exclamationMarkChar: Parser[RegexAtomElement] = "!" ^^^ ExclamationMarkChar

  def commaChar: Parser[RegexAtomElement] = "," ^^^ CommaChar

  def plusChar: Parser[String] = "+"

  def starChar: Parser[String] = "*"

  def questionMarkChar: Parser[String] = "?"

  def dotChar: Parser[RegexAtomElement] = "." ^^^ DotChar()

  def digitChar = regex("[0-9]") ^^ (v => DigitChar(v.charAt(0)))

  def otherChar: Parser[OtherChar] = (not(escapeChar | andChar | orChar | hyphenChar | caretChar | colonChar | dollarChar | squareBracketStartChar |
    squareBracketEndChar | roundBracketStartChar | curlyBracketEndChar | equalsChar | lessThanChar | greaterThanChar | exclamationMarkChar |
    commaChar | plusChar | starChar | questionMarkChar | dotChar | digitChar) ~> regex(".")) ^^ (v => OtherChar(v.charAt(0)))
  
  def startOfFile = regex("^")
  
  def endOfFile = regex("$")
}