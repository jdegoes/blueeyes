package blueeyes.util

import scala.util.parsing.combinator._
import scala.util.matching.Regex

object RegularExpressionAST {
  case class RegularExpression(atoms: RegexAtom*)
  
  sealed trait RegexAtom
  
  abstract class SingleChar extends RegexAtom { def char: Char } 
  abstract class FixedSingleChar(val char: Char) extends SingleChar
  case object AndChar extends FixedSingleChar('&')
  case object HyphenChar extends FixedSingleChar('-')
  case object ColonChar extends FixedSingleChar(':')
  case object SquareBracketEndChar extends FixedSingleChar(':')
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
  
  sealed trait BoundaryMatch extends RegexAtom
  case object Caret extends BoundaryMatch
  case object Dollar extends BoundaryMatch
  case class BoundaryMatchEscape(escape: Char) extends BoundaryMatch
  
  
  
  sealed trait ShorthandCharacterClass extends RegexAtom
  case object DigitClass    extends ShorthandCharacterClass
  case object NotDigitClass extends ShorthandCharacterClass
  case object SpaceClass    extends ShorthandCharacterClass
  case object NotSpaceClass extends ShorthandCharacterClass
  case object WordClass     extends ShorthandCharacterClass
  case object NotWordClass  extends ShorthandCharacterClass
  
  sealed trait PosixCharacterClass extends RegexAtom
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
  
  case class EscapeSequence() extends RegexAtom
  case class CharacterClass() extends RegexAtom
  case class FlagGroup() extends RegexAtom
  case class Quotation() extends RegexAtom
  case class NamedCaptureGroup() extends RegexAtom
  case class NonCapturingGroup() extends RegexAtom
  case class AtomicGroup() extends RegexAtom
  case class LookAround() extends RegexAtom
  case class Group() extends RegexAtom
  case class BackReference(reference: Int) extends RegexAtom
  case class DotChar() extends RegexAtom
  case class OrChar() extends RegexAtom
  
}

trait RegularExpressionGrammar extends RegexParsers {
  private[this] implicit def string2Regex(s: String): Regex = new Regex(s)
  
  def regularExpression = startOfFile ~ (regexAtom*) ~ endOfFile

  def regexAtom: Parser[Any] = (
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
  ) ~ (quantifier?)

  def backReference = escapeChar ~ (digitChar+)

  def singleChar = (andChar | 
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

  def atomicGroup = roundBracketStartChar ~ questionMarkChar ~ greaterThanChar ~ (regexAtom*) ~ roundBracketEndChar

  def flagGroup = roundBracketStartChar ~ questionMarkChar ~ flags ~ roundBracketEndChar

  def flags = (otherChar*) ~ (hyphenChar ~ (otherChar*))?

  def lookAround = roundBracketStartChar ~ questionMarkChar ~ (lessThanChar?) ~ (equalsChar | exclamationMarkChar) ~ (regexAtom*) ~ roundBracketEndChar
  
  def group = roundBracketStartChar ~ (regexAtom*) ~ roundBracketEndChar
  
  def namedCaptureGroup: Parser[Any] = roundBracketStartChar ~ questionMarkChar ~ lessThanChar ~ (regex("[a-zA-z0-9]")+) ~ greaterThanChar ~ (regexAtom*) ~ roundBracketEndChar

  def nonCapturingGroup: Parser[Any] = roundBracketStartChar ~ questionMarkChar ~ (flags?) ~ colonChar ~ (regexAtom*) ~ roundBracketEndChar

  def characterClass = squareBracketStartChar ~ (caretChar?) ~ (squareBracketEndChar ~ (characterClassAtom*) | (characterClassAtom+)) ~ squareBracketEndChar

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

  def quantifier = reluctantQuantifier | possessiveQuantifier | greedyQuantifier

  def greedyQuantifier = questionMarkChar | starChar | plusChar | 
    (curlyBracketStartChar ~ (decimal | (decimal ~ commaChar) | (decimal ~ commaChar ~ decimal)) ~ curlyBracketEndChar)

  def reluctantQuantifier = greedyQuantifier ~ questionMarkChar

  def possessiveQuantifier = greedyQuantifier ~ plusChar

  def decimal = digitChar+

  def boundaryMatch = caretChar | dollarChar | boundaryMatchEscape

  def quotation = quotationStart ~ ((quotationStart | 
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
  )*) ~ quotationEnd

  def quotationStart = "\\q"

  def quotationEnd = "\\e"

  def posixCharacterClass = "\\p{" ~ ("lower" |
    "upper" |
    "aSCII" |
    "alpha" |
    "digitChar" |
    "alnum" |
    "punct" |
    "graph" |
    "print" |
    "blank" |
    "cntrl" |
    "xDigit" |
    "space"
  ) ~ "}"

  def shorthandCharacterClass = escapeChar ~ regex("[dDsSwW]")

  def boundaryMatchEscape = escapeChar ~ regex("[bBAzZG]")

  def octalNumber = escapeChar ~ "0" ~ (
    ((regex("[0-7]")?) ~ regex("[0-7]")) |
    (regex("[0-3]") ~ regex("[0-7]") ~ regex("[0-7]"))
  )

  def smallHexNumber = escapeChar ~ "x" ~ regex("[0-9a-fA-f]{2}")

  def unicodeChar = escapeChar ~ "u" ~ regex("[0-9a-fA-f]{4}")

  def escapeSequence = escapeChar ~ regex("[tnrfae&&[^a-zA-z0-9]]")

  def escapeChar: Parser[String] = "\\"

  def andChar: Parser[String] = "&"

  def orChar: Parser[String] = "|"

  def hyphenChar: Parser[String] = "-"

  def caretChar: Parser[String] = "^"

  def colonChar: Parser[String] = ":"

  def dollarChar: Parser[String] = "$"

  def squareBracketStartChar: Parser[String] = "["

  def squareBracketEndChar: Parser[String] = "]"

  def roundBracketStartChar: Parser[String] = "("

  def roundBracketEndChar: Parser[String] = ")"

  def curlyBracketStartChar: Parser[String] = "{"

  def curlyBracketEndChar: Parser[String] = "}"

  def equalsChar: Parser[String] = "="

  def lessThanChar: Parser[String] = "<"

  def greaterThanChar: Parser[String] = ">"

  def exclamationMarkChar: Parser[String] = "!"

  def commaChar: Parser[String] = ","

  def plusChar: Parser[String] = "+"

  def starChar: Parser[String] = "*"

  def questionMarkChar: Parser[String] = "?"

  def dotChar: Parser[String] = "."

  def digitChar = regex("[0-9]")

  def otherChar = not(escapeChar | andChar | orChar | hyphenChar | caretChar | colonChar | dollarChar | squareBracketStartChar | 
    squareBracketEndChar | roundBracketStartChar | curlyBracketEndChar | equalsChar | lessThanChar | greaterThanChar | exclamationMarkChar |
    commaChar | plusChar | starChar | questionMarkChar | dotChar | digitChar)
  
  def startOfFile = regex("^")
  
  def endOfFile = regex("$")
}