package blueeyes.util

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.matching.Regex

object RegularExpressionAST {
  case class RegularExpression(atoms: RegexAtom*)

  case class RegexAtom(element: RegexAtomElement, quantifier: Option[Quantifier]){
    override def toString = element.toString + quantifier.map(_.toString).getOrElse("")
  }
  sealed trait RegexAtomElement
  sealed trait CharacterClassAtom
  sealed trait QuotationAtom

  sealed trait Quantifier
  case class GreedyQuantifier(lowerBound: Int, upperBound: Option[Int]) extends Quantifier{
    override def toString = (lowerBound, upperBound) match{
      case (0, Some(1)) => "?"
      case (0, None) => "*"
      case (1, None) => "+"
      case (lower, None) => "{%s,}".format(lower)
      case (lower, Some(upper)) if (lower == upper) => "{%s}".format(lower)
      case (lower, Some(upper)) => "{%s,%s}".format(lower, upper)
    }
  }
  case class ReluctantQuantifier(quantifier: GreedyQuantifier) extends Quantifier{
    override def toString = quantifier + "?"
  }
  case class PossessiveQuantifier(quantifier: GreedyQuantifier) extends Quantifier{
    override def toString = quantifier + "+"
  }

  abstract class CharWrapper{
    def char: Char

    override def toString = char.toString
  }
  abstract class SingleChar extends CharWrapper with RegexAtomElement
  abstract class FixedSingleChar(val char: Char) extends SingleChar
  case object AndChar extends FixedSingleChar('&') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object HyphenChar extends FixedSingleChar('-') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object ColonChar extends FixedSingleChar(':') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object SquareBracketEndChar extends FixedSingleChar(']') with CharacterClassAtom with QuotationAtom
  case object CurlyBracketEndChar extends FixedSingleChar('}') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object EqualsChar extends FixedSingleChar('=') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object LessThanChar extends FixedSingleChar('<') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object GreaterThanChar extends FixedSingleChar('>') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object ExclamationMarkChar extends FixedSingleChar('!') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object CommaChar extends FixedSingleChar(',') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom

  abstract class NonSingleChar(val char: Char) extends CharWrapper
  case object CaretChar extends NonSingleChar('^') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object DollarChar extends NonSingleChar('$') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object SquareBracketStartChar extends NonSingleChar('[') with QuotationAtom
  case object CurlyBracketStartChar extends NonSingleChar('{') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object RoundBracketStartChar extends NonSingleChar('(') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object RoundBracketEndChar extends NonSingleChar(')') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object EscapeChar extends NonSingleChar('\\') with QuotationAtom
  case object PlusChar extends NonSingleChar('+') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object StarChar extends NonSingleChar('*') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object QuestionMarkChar extends NonSingleChar('?') with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object DotChar extends NonSingleChar('.') with RegexAtomElement with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  case object OrChar extends NonSingleChar('|') with RegexAtomElement with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom

  case class DigitChar(char: Char) extends SingleChar with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  abstract class HexNumber extends SingleChar with CharacterClassAtom with QuotationAtom{
    def size: Int
    def prefix: String

    override def toString = {
      val hex     = Integer.toHexString(char.toInt)
      val missing = List.fill(size - hex.length)("0")
      prefix + missing.mkString + hex
    }    
  }
  case class SmallHexNumber(char: Char) extends HexNumber with CharacterClassRangeAtom{
    val size   = 2
    val prefix = "\\x"
  }
  case class UnicodeChar(char: Char) extends HexNumber with CharacterClassRangeAtom{
    val size   = 4
    val prefix = "\\u"
  }
  case class OctalNumber(char: Char) extends SingleChar with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom{
    override def toString = "\\0" + Integer.toOctalString(char.toInt)
  }
  case class OtherChar(char: Char) extends SingleChar with CharacterClassAtom with QuotationAtom with CharacterClassRangeAtom
  
  sealed trait BoundaryMatch extends RegexAtomElement with QuotationAtom
  case object Caret extends BoundaryMatch{
    override def toString = "^"
  }
  case object Dollar extends BoundaryMatch{
    override def toString = "$"
  }
  case class BoundaryMatchEscape(boundaryMatch: Char) extends BoundaryMatch{
    override def toString = "\\" + boundaryMatch
  }

  sealed abstract class ShorthandCharacterClass(char: Char) extends RegexAtomElement with CharacterClassAtom with QuotationAtom{
    override def toString = "\\" + char
  }
  case object DigitClass    extends ShorthandCharacterClass('d')
  case object NotDigitClass extends ShorthandCharacterClass('D')
  case object SpaceClass    extends ShorthandCharacterClass('s')
  case object NotSpaceClass extends ShorthandCharacterClass('S')
  case object WordClass     extends ShorthandCharacterClass('w')
  case object NotWordClass  extends ShorthandCharacterClass('W')
  
  sealed abstract class PosixCharacterClass(name: String) extends RegexAtomElement with CharacterClassAtom with QuotationAtom{
    override def toString = "\\p{" + name + "}"
  }

  case object PosixLower extends PosixCharacterClass("Lower")
  case object PosixUpper extends PosixCharacterClass("Upper")
  case object PosixASCII extends PosixCharacterClass("ASCII")
  case object PosixAlpha extends PosixCharacterClass("Alpha")
  case object PosixDigit extends PosixCharacterClass("Digit")
  case object PosixAlnum extends PosixCharacterClass("Alnum")
  case object PosixPunct extends PosixCharacterClass("Punct")
  case object PosixGraph extends PosixCharacterClass("Graph")
  case object PosixPrint extends PosixCharacterClass("Print")
  case object PosixBlank extends PosixCharacterClass("Blank")
  case object PosixCntrl extends PosixCharacterClass("Cntrl")
  case object PosixXDigit extends PosixCharacterClass("XDigit")
  case object PosixSpace extends PosixCharacterClass("Space")
  
  case class EscapeSequence(character: Char) extends RegexAtomElement with CharacterClassAtom with QuotationAtom{
    override def toString = "\\" + character
  }
  case class CharacterClass(negated: Boolean, characterClassAtoms: CharacterClassAtom*) extends RegexAtomElement{
    override def toString = "[" + (if (negated) "^" else "") + characterClassAtoms.mkString("") + "]"
  }
  case class FlagGroup(flags: Flags) extends RegexAtomElement{
    override def toString = "(?" + flags.toString + ")"
  }
  case class Flags(flags1: List[Char], flags2: Option[List[Char]]){
    override def toString = flags1.mkString("") + flags2.map(v => "-" + v.mkString("")).getOrElse("")
  }
  case class Quotation(quotations: QuotationAtom*) extends RegexAtomElement with CharacterClassAtom{
    override def toString = quotations.mkString(QuotationStart.toString, "", QuotationEnd.toString)   
  }
  case class NamedCaptureGroup(name: String, group: RegexAtom*) extends RegexAtomElement{
    override def toString = "(?<" + name + ">" + group.mkString("") + ")"
  }
  case class NonCapturingGroup(flags: Option[Flags], group: RegexAtom*) extends RegexAtomElement{
    override def toString = "(?" + flags.map(_.toString).getOrElse("") + ":" + group.mkString("") + ")"
  }
  case class AtomicGroup(group: RegexAtom*) extends RegexAtomElement{
    override def toString = "(?>" + group.mkString("") + ")"
  }
  case class LookAround(ahead: Boolean, positive: Boolean, regex: RegexAtom*) extends RegexAtomElement{
    override def toString = "(?" + (if (ahead) "" else "<") + (if (positive) "=" else "!") + regex.mkString("") + ")"    
  }
  case class Group(group: RegexAtom*) extends RegexAtomElement{
    override def toString = "(" + group.mkString("") + ")"
  }
  case class BackReference(reference: Int) extends RegexAtomElement{
    override def toString = "\\" + reference.toString
  }

  case class CharacterClassRange(start: CharacterClassRangeAtom, end: CharacterClassRangeAtom) extends CharacterClassAtom{
    override def toString = start.toString + "-" + end.toString
  }

  trait CharacterClassRangeAtom

  case object QuotationStart extends QuotationAtom{
    override def toString = "\\Q"
  }
  case object QuotationEnd{
    override def toString = "\\E"
  }
}

trait RegularExpressionGrammar extends RegexParsers {
  import RegularExpressionAST._

  private[this] implicit def string2Regex(s: String): Regex = new Regex(s)
  
  def regularExpression: Parser[List[RegexAtom]] = startOfFile ~> (regexAtom*) <~ endOfFile

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

  def backReference = escapeChar ~> (regex("[1-9]")+) ^^ {value => BackReference(value.mkString("").toInt)}

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

  def atomicGroup = roundBracketStartChar ~> questionMarkChar ~> greaterThanChar ~> (not(roundBracketEndChar) ~> regexAtom*) <~ roundBracketEndChar ^^ {value => AtomicGroup(value: _*)}

  def flagGroup = roundBracketStartChar ~> (questionMarkChar ~> flags) <~ roundBracketEndChar ^^ (value => FlagGroup(value))

  def flags = (regex("[sim]")*) ~ ((hyphenChar ~> (regex("[sim]")*))?) ^^ {case flags1 ~ flags2 => Flags(flags1.map(_.charAt(0)), flags2.map(_.map(_.charAt(0))))}

  def lookAround = roundBracketStartChar ~> questionMarkChar ~> (lessThanChar?) ~ (equalsChar | exclamationMarkChar) ~ (not(roundBracketEndChar) ~> regexAtom*) <~ roundBracketEndChar ^^ {case lookbehind ~ positive ~ value => LookAround(lookbehind.map(v => false).getOrElse(true), positive == EqualsChar, value: _*)}
  
  def group = roundBracketStartChar ~> (not(roundBracketEndChar) ~> regexAtom*) <~ roundBracketEndChar ^^ {value => Group(value: _*)}
  
  def namedCaptureGroup = roundBracketStartChar ~> questionMarkChar ~> (lessThanChar ~> (regex("[a-zA-z0-9]")+) <~ greaterThanChar) ~ (not(roundBracketEndChar) ~> regexAtom*) <~ roundBracketEndChar  ^^ {case name ~ value => NamedCaptureGroup(name.mkString(""), value: _*)}

  def nonCapturingGroup = roundBracketStartChar ~> questionMarkChar ~> (flags?) ~ (colonChar ~> (not(roundBracketEndChar) ~> regexAtom*)) <~ roundBracketEndChar ^^ {case flags ~ value => NonCapturingGroup(flags, value: _*)}

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

  def characterClassRange = rangeAtom ~ hyphenChar ~ rangeAtom ^^ {case start ~ hyphen ~ end => CharacterClassRange(start, end)}

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

  def quotationStart = "\\Q" ^^^ QuotationStart

  def quotationEnd = "\\E" ^^^ QuotationEnd

  def posixCharacterClass: Parser[PosixCharacterClass] = "\\p{" ~> ("Lower" ^^^ PosixLower |
    "Upper" ^^^ PosixUpper |
    "ASCII" ^^^ PosixASCII |
    "Alpha" ^^^ PosixAlpha |
    "Digit" ^^^ PosixDigit |
    "Alnum" ^^^ PosixAlnum |
    "Punct" ^^^ PosixPunct |
    "Graph" ^^^ PosixGraph |
    "Print" ^^^ PosixPrint |
    "Blank" ^^^ PosixBlank |
    "Cntrl" ^^^ PosixCntrl |
    "XDigit" ^^^ PosixXDigit |
    "Space" ^^^ PosixSpace
  ) <~ "}"

  def shorthandCharacterClass: Parser[ShorthandCharacterClass] = escapeChar ~> ("d" ^^^ DigitClass |
    "D" ^^^ NotDigitClass |
    "s" ^^^ SpaceClass |
    "S" ^^^ NotSpaceClass |
    "w" ^^^ WordClass |
    "W" ^^^ NotWordClass
  )

  def boundaryMatchEscape = escapeChar ~> regex("[bBAzZG]") ^^ {char => BoundaryMatchEscape(char.charAt(0))}

  def octalNumber = escapeChar ~> "0" ~> (
    (regex("[0-7]{1,2}") ^^ {v => parseOctal(v)}) |
    (regex("[0-3]") ~ regex("[0-7]") ~ regex("[0-7]") ^^ {case v1~v2~v3 => parseOctal(v1 + v2 + v3)})
  )

  private def parseOctal(v: String) = OctalNumber(Integer.parseInt(v, 8).toChar)

  def smallHexNumber = escapeChar ~> "x" ~> regex("[0-9a-fA-f]{2}") ^^ {v => SmallHexNumber(Integer.parseInt(v, 16).toChar)}

  def unicodeChar = escapeChar ~> "u" ~> regex("[0-9a-fA-f]{4}") ^^ {v => UnicodeChar(Integer.parseInt(v, 16).toChar)}

  def escapeSequence = escapeChar ~> (regex("[tnrfae]") | (not(regex("[0-9a-fA-f]")) ~> regex(".") )) ^^ (character => EscapeSequence(character.charAt(0)))

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

import RegularExpressionAST._
object RegularExpressionPatten extends PartialFunction[String, List[RegexAtom]] with RegularExpressionGrammar with RegexParsers{
  def isDefinedAt(s: String): Boolean = regularExpression.apply(new CharSequenceReader(s)) match {
    case Success(result, _) => true

    case _ => false
  }
  def apply(s: String): List[RegexAtom] = regularExpression.apply(new CharSequenceReader(s)) match {
    case Success(result, _) => result

    case Failure(msg, _)    => parseFailure(msg, s)

    case Error(msg, _)      => parseError(msg, s)
  }

  private def parseFailure(msg: String, s: String) = {
    error("The pattern " + this.toString + " does not match " + s + ": " + msg)
  }
  private def parseError(msg: String, s: String) = {
    error("There was an error parsing \"" + s + "\" with pattern \"" + this.toString + "\": " + msg)
  }  
}