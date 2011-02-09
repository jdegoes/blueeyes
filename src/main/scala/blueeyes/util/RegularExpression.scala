package blueeyes.util

import scala.util.parsing.combinator._
import scala.util.matching.Regex

object RegularExpressionAST {
  case class RegularExpression(atoms: RegexAtom*)

  case class RegexAtom(element: RegexAtomElement, quantifier: Option[Quantifier])
  sealed trait RegexAtomElement
  sealed trait CharacterClassAtom
  sealed trait QuotationAtom

  sealed trait Quantifier
  case class GreedyQuantifier(lowerBound: Int, upperBound: Option[Int]) extends Quantifier
  case class ReluctantQuantifier(quantifier: GreedyQuantifier) extends Quantifier
  case class PossessiveQuantifier(quantifier: GreedyQuantifier) extends Quantifier

  abstract class CharWrapper{def char: Char}
  abstract class SingleChar extends CharWrapper with RegexAtomElement
  abstract class FixedSingleChar(val char: Char) extends SingleChar
  case object AndChar extends FixedSingleChar('&') with CharacterClassAtom with QuotationAtom
  case object HyphenChar extends FixedSingleChar('-') with CharacterClassAtom with QuotationAtom
  case object ColonChar extends FixedSingleChar(':') with CharacterClassAtom with QuotationAtom
  case object SquareBracketEndChar extends FixedSingleChar(']') with CharacterClassAtom with QuotationAtom
  case object CurlyBracketEndChar extends FixedSingleChar('}') with CharacterClassAtom with QuotationAtom
  case object EqualsChar extends FixedSingleChar('=') with CharacterClassAtom with QuotationAtom
  case object LessThanChar extends FixedSingleChar('<') with CharacterClassAtom with QuotationAtom
  case object GreaterThanChar extends FixedSingleChar('>') with CharacterClassAtom with QuotationAtom
  case object ExclamationMarkChar extends FixedSingleChar('!') with CharacterClassAtom with QuotationAtom
  case object CommaChar extends FixedSingleChar(',') with CharacterClassAtom with QuotationAtom

  abstract class NonSingleChar(val char: Char) extends CharWrapper
  case object CaretChar extends NonSingleChar('^') with CharacterClassAtom with QuotationAtom
  case object DollarChar extends NonSingleChar('$') with CharacterClassAtom with QuotationAtom
  case object SquareBracketStartChar extends NonSingleChar('[') with QuotationAtom
  case object CurlyBracketStartChar extends NonSingleChar('{') with CharacterClassAtom with QuotationAtom
  case object RoundBracketStartChar extends NonSingleChar('(') with CharacterClassAtom with QuotationAtom
  case object RoundBracketEndChar extends NonSingleChar(')') with CharacterClassAtom with QuotationAtom
  case object EscapeChar extends NonSingleChar('\\') with QuotationAtom
  case object PlusChar extends NonSingleChar('+') with CharacterClassAtom with QuotationAtom
  case object StarChar extends NonSingleChar('*') with CharacterClassAtom with QuotationAtom
  case object QuestionMarkChar extends NonSingleChar('?') with CharacterClassAtom with QuotationAtom
  case object DotChar extends NonSingleChar('.') with RegexAtomElement with CharacterClassAtom with QuotationAtom
  case object OrChar extends NonSingleChar('|') with RegexAtomElement with CharacterClassAtom with QuotationAtom
  
  case class DigitChar(char: Char) extends SingleChar with CharacterClassAtom with QuotationAtom
  case class SmallHexNumber(char: Char) extends SingleChar with CharacterClassAtom with QuotationAtom
  case class UnicodeChar(char: Char) extends SingleChar with CharacterClassAtom with QuotationAtom
  case class OctalNumber(char: Char) extends SingleChar with CharacterClassAtom with QuotationAtom
  case class OtherChar(char: Char) extends SingleChar with CharacterClassAtom with QuotationAtom
  
  sealed trait BoundaryMatch extends RegexAtomElement with QuotationAtom
  case object Caret extends BoundaryMatch
  case object Dollar extends BoundaryMatch
  case class BoundaryMatchEscape(escape: Char) extends BoundaryMatch

  sealed trait ShorthandCharacterClass extends RegexAtomElement with CharacterClassAtom with QuotationAtom
  case object DigitClass    extends ShorthandCharacterClass
  case object NotDigitClass extends ShorthandCharacterClass
  case object SpaceClass    extends ShorthandCharacterClass
  case object NotSpaceClass extends ShorthandCharacterClass
  case object WordClass     extends ShorthandCharacterClass
  case object NotWordClass  extends ShorthandCharacterClass
  
  sealed trait PosixCharacterClass extends RegexAtomElement with CharacterClassAtom with QuotationAtom
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
  
  case class EscapeSequence(character: Char) extends RegexAtomElement with CharacterClassAtom with QuotationAtom
  case class CharacterClass(negated: Boolean, characterClassAtoms: CharacterClassAtom*) extends RegexAtomElement
  case class FlagGroup(flags: Flags) extends RegexAtomElement
  case class Flags(flags1: List[Char], flags2: Option[List[Char]])
  case class Quotation(quotations: QuotationAtom*) extends RegexAtomElement with CharacterClassAtom
  case class NamedCaptureGroup(name: String, group: RegexAtom*) extends RegexAtomElement
  case class NonCapturingGroup(flags: Option[Flags], group: RegexAtom*) extends RegexAtomElement
  case class AtomicGroup(group: RegexAtom*) extends RegexAtomElement
  case class LookAround(ahead: Boolean, positive: Boolean, regex: RegexAtom*) extends RegexAtomElement
  case class Group(group: RegexAtom*) extends RegexAtomElement
  case class BackReference(reference: Int) extends RegexAtomElement

  case class CharacterClassRange(start: Char, end: Char) extends CharacterClassAtom

  case object QuotationStart extends QuotationAtom
  case object QuotationEnd
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

  def backReference = escapeChar ~> (decimal) ^^ {value => BackReference(value)}

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

  def atomicGroup = roundBracketStartChar ~> questionMarkChar ~> greaterThanChar ~> (regexAtom*) <~ roundBracketEndChar ^^ {value => AtomicGroup(value: _*)}

  def flagGroup = roundBracketStartChar ~> questionMarkChar ~> flags <~ roundBracketEndChar ^^ (value => FlagGroup(value))

  def flags = (otherChar*) ~ ((hyphenChar ~> (otherChar*))?) ^^ {case flags1 ~ flags2 => Flags(flags1.map(_.char), flags2.map(_.map(_.char)))}

  def lookAround = roundBracketStartChar ~> questionMarkChar ~> (lessThanChar?) ~ (equalsChar | exclamationMarkChar) ~ (regexAtom*) <~ roundBracketEndChar ^^ {case lookbehind ~ negative ~ value => LookAround(lookbehind.map(v => false).getOrElse(true), negative != ExclamationMarkChar, value: _*)}
  
  def group = roundBracketStartChar ~> (regexAtom*) <~ roundBracketEndChar ^^ {value => Group(value: _*)}
  
  def namedCaptureGroup = roundBracketStartChar ~> questionMarkChar ~> (lessThanChar ~> (regex("[a-zA-z0-9]")+) <~ greaterThanChar) ~ (regexAtom*) <~ roundBracketEndChar  ^^ {case name ~ value => NamedCaptureGroup(name.mkString(""), value: _*)}

  def nonCapturingGroup = roundBracketStartChar ~> questionMarkChar ~> (flags?) ~ (colonChar ~> (regexAtom*)) <~ roundBracketEndChar ^^ {case flags ~ value => NonCapturingGroup(flags, value: _*)}

  def characterClass = squareBracketStartChar ~> (caretChar?) ~ ((squareBracketEndChar ~ (characterClassAtom*) ^^ {case end ~ atoms  => end :: atoms}) | (characterClassAtom+)) <~ squareBracketEndChar ^^ {case caret ~ atoms => CharacterClass(caret.map(v => true).getOrElse(false), atoms: _*)}

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

  def characterClassRange = rangeAtom ~ hyphenChar ~ rangeAtom ^^ {case start ~ hyphen ~ end => CharacterClassRange(start.char, end.char)}

  def rangeAtom: Parser[CharWrapper] = smallHexNumber |
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

  def greedyQuantifier = questionMarkChar ^^^ GreedyQuantifier(0, Some(1))  | starChar ^^^ GreedyQuantifier(0, None) | plusChar ^^^ GreedyQuantifier(1, None) |
    (curlyBracketStartChar ~> (decimal ^^ {value => GreedyQuantifier(value, Some(value))} | (decimal <~ commaChar) ^^ {value => GreedyQuantifier(value, None)} | (decimal ~ commaChar ~ decimal) ^^ {case lower ~ comma ~ upper => GreedyQuantifier(lower, Some(upper))}) <~ curlyBracketEndChar)

  def reluctantQuantifier = greedyQuantifier <~ questionMarkChar ^^ (value => ReluctantQuantifier(value))

  def possessiveQuantifier = greedyQuantifier <~ plusChar ^^ (value => PossessiveQuantifier(value))

  def decimal: Parser[Int] = (digitChar+) ^^ (digitChars => digitChars.map(_.char).mkString("").toInt)

  def boundaryMatch: Parser[BoundaryMatch] = caretChar ^^^ Caret | dollarChar ^^^ Dollar | boundaryMatchEscape

  def quotation = quotationStart ~> ((quotationStart |
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
  )*) <~ quotationEnd ^^ (quotations => Quotation(quotations: _*))

  def quotationStart = "\\q" ^^^ QuotationStart

  def quotationEnd = "\\e" ^^^ QuotationEnd

  def posixCharacterClass: Parser[PosixCharacterClass] = "\\p{" ~> ("lower" ^^^ PosixLower |
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

  def shorthandCharacterClass: Parser[ShorthandCharacterClass] = escapeChar ~> ("d" ^^^ DigitClass |
    "D" ^^^ NotDigitClass |
    "s" ^^^ SpaceClass |
    "S" ^^^ NotSpaceClass |
    "w" ^^^ WordClass |
    "W" ^^^ NotWordClass
  )

  def boundaryMatchEscape = escapeChar ~> regex("[bBAzZG]") ^^ {char => BoundaryMatchEscape(char.charAt(0))}

  def octalNumber = escapeChar ~ "0" ~> (
    ((regex("[0-7]")?) ~ regex("[0-7]") ^^ {case v1~v2 => parseOctal(v1.getOrElse("") + v2)}) |
    (regex("[0-3]") ~ regex("[0-7]") ~ regex("[0-7]") ^^ {case v1~v2~v3 => parseOctal(v1 + v2 + v3)})
  )

  private def parseOctal(v: String) = OctalNumber(Integer.parseInt(v, 8).toChar)

  def smallHexNumber = escapeChar ~> "x" ~> regex("[0-9a-fA-f]{2}") ^^ {v => SmallHexNumber(Integer.parseInt(v, 16).toChar)}

  def unicodeChar = escapeChar ~> "u" ~> regex("[0-9a-fA-f]{4}") ^^ {v => UnicodeChar(Integer.parseInt(v, 16).toChar)}

  def escapeSequence = escapeChar ~> regex("[tnrfae&&[^a-zA-z0-9]]") ^^ (character => EscapeSequence(character.charAt(0)))

  def escapeChar = "\\" ^^^ EscapeChar

  def andChar = "&" ^^^ AndChar

  def orChar = "|" ^^^ OrChar

  def hyphenChar = "-" ^^^ HyphenChar

  def caretChar = "^" ^^^ CaretChar

  def colonChar = ":" ^^^ ColonChar

  def dollarChar = "$" ^^^ DollarChar

  def squareBracketStartChar = "[" ^^^ SquareBracketStartChar

  def squareBracketEndChar = "]" ^^^ SquareBracketEndChar

  def roundBracketStartChar = "(" ^^^ RoundBracketStartChar

  def roundBracketEndChar = ")" ^^^ RoundBracketEndChar

  def curlyBracketStartChar = "{" ^^^ CurlyBracketStartChar

  def curlyBracketEndChar = "}" ^^^ CurlyBracketEndChar

  def equalsChar = "=" ^^^ EqualsChar

  def lessThanChar = "<" ^^^ LessThanChar

  def greaterThanChar = ">" ^^^ GreaterThanChar

  def exclamationMarkChar = "!" ^^^ ExclamationMarkChar

  def commaChar = "," ^^^ CommaChar

  def plusChar = "+" ^^^ PlusChar

  def starChar = "*" ^^^ StarChar

  def questionMarkChar = "?" ^^^ QuestionMarkChar

  def dotChar = "." ^^^ DotChar

  def digitChar = regex("[0-9]") ^^ (v => DigitChar(v.charAt(0)))

  def otherChar = (not(escapeChar | andChar | orChar | hyphenChar | caretChar | colonChar | dollarChar | squareBracketStartChar |
    squareBracketEndChar | roundBracketStartChar | curlyBracketEndChar | equalsChar | lessThanChar | greaterThanChar | exclamationMarkChar |
    commaChar | plusChar | starChar | questionMarkChar | dotChar | digitChar) ~> regex(".")) ^^ (v => OtherChar(v.charAt(0)))
  
  def startOfFile = regex("^")
  
  def endOfFile = regex("$")
}