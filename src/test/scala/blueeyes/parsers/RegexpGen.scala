package blueeyes.parsers

import org.scalacheck._
import Gen._
import scala.util.Random

object RegexpGen{
  private val random              = new Random()
  private val excludeAnotherChars = "\\&|-^:$[](){}=<>!,+*?.0123456789"

  def singleFixedChar = oneOf("&", "-", ":", "]", "}", "=", "<", ">", "!", ",")

  def boundaryMatch = oneOf("$", "^", "\\b", "\\B", "\\A", "\\z", "\\Z", "\\G")

  def shorthandCharacterClass = oneOf("\\s", "\\S", "\\d", "\\D", "\\w", "\\W")
  
  def escapeSequence = oneOf("\\t", "\\n", "\\r", "\\f", "\\a", "\\e")

  def posixCharacterClass = oneOf("\\p{Lower}", "\\p{Upper}", "\\p{ASCII}", "\\p{Alpha}", "\\p{Digit}", "\\p{Alnum}", "\\p{Punct}", "\\p{Graph}", "\\p{Print}", "\\p{Blank}", "\\p{Cntrl}", "\\p{XDigit}", "\\p{Space}")

  def digit = for (l <- numChar) yield l.toString

  def smallHexNumber = listOfN(2, hexDigit).map(v => "\\x" + v.mkString(""))

  def smallLowerCaseHexNumber = smallHexNumber.map(_.toLowerCase)

  def unicodeChar    = listOfN(4, hexDigit).map(v => "\\u" + v.mkString(""))

  def unicodeLowerCaseChar = unicodeChar.map(_.toLowerCase)

  def otherChar      = frequency((1,alphaUpperChar), (1,alphaLowerChar)).map(v => if (excludeAnotherChars.contains(v.toString)) "a" else v.toString)

  def flagGroup = {
    for{
      flags <- flags
    } yield ("(?" + flags + ")")
  }

  def flags = {
    def flag = oneOf("s", "i", "m")
    for{
      withHyphen <- genBool
      size       <- choose(1, 3)
      flags1     <- flag
      flags2     <- flag
    } yield ( flags1.mkString("") + (if (withHyphen) "-" + flags2.mkString("") else ""))
  }

  def octalNumber = {
    def octalDigit = choose(48, 55).map(_.toChar)
    for{
      size <- oneOf(1, 2, 3)
      l    <- choose(48, 51).map(_.toChar)
      h    <- listOfN(2, octalDigit)
    } yield {
      val value = (l :: h).takeRight(size).filter(_ != '0').mkString("")
      "\\0" + (if (value == "") "0" else value)
    }
  }

  def octalNumberWithoutZero = octalNumber.map(v => if (v == "\\00") "\\01" else v)

  def quotation = {
    for {
      size      <- choose(1, 20)
      quotation <- listOfN(size, oneOf(oneOf("\\Q", "&", "|", "-", "^", ":", "$", "[", "]", "(", ")", "{", "}", "=", "<", ">", "!", ",", "+", "*", "?", "."), digit, otherChar, posixCharacterClass, shorthandCharacterClass, boundaryMatch, octalNumberWithoutZero, smallLowerCaseHexNumber, unicodeLowerCaseChar, escapeSequence))
    } yield (quotation.mkString("\\Q", "", "\\E"))
  }

  def singleChar = oneOf(singleFixedChar, digit, smallLowerCaseHexNumber, unicodeLowerCaseChar, octalNumberWithoutZero, otherChar)

  def regexAtom: Gen[String] = {
    for{
      withQuantifier   <- genBool
      regexAtomElement <- oneOf(singleChar, boundaryMatch, shorthandCharacterClass, posixCharacterClass, escapeSequence, characterClass, flagGroup, quotation, namedCaptureGroup, nonCapturingGroup, atomicGroup, lookAround, group,  backReference, oneOf(".", "|"))
      quantifier       <- quantifier
    } yield ( regexAtomElement + (if (withQuantifier) quantifier else "") )
  }

  def backReference = {
    for {
      size   <- choose(1, 4)
      values <- listOfN(size, choose(49, 57))
    } yield ("\\" + values.mkString(""))
  }
  
  def quantifier = oneOf(reluctantQuantifier, possessiveQuantifier, greedyQuantifier)

  def reluctantQuantifier = greedyQuantifier.map(_ + "?")
  
  def possessiveQuantifier = greedyQuantifier.map(_ + "+")

  def greedyQuantifier = {
    def count  = choose(1, 10).map("{" + _ + "}")
    def count1 = choose(1, 10).map("{" + _ + ",}")
    def count2 = choose(1, 10).map(v => "{" + v + "," + v + "}")

    oneOf(oneOf("?", "*", "+"), count, count1, count2)
  }

  def characterClass = {
    for {
      withCaret            <- genBool
      withSquareBracketEnd <- genBool
      size                 <- choose(1, 10)
      generateSize         <- if (withSquareBracketEnd) size - 1 else size
      values               <- listOfN(generateSize, characterClassAtom)
    } yield {
      val caret             = if (withCaret) "^" else ""
      val squareBracketEnd  = if (withSquareBracketEnd) "]" else ""
      values.mkString("[" + caret + squareBracketEnd, "", "]")
    }
  }

  def characterClassAtom = oneOf(characterClassRange, escapeSequence, shorthandCharacterClass, posixCharacterClass, quotation, octalNumberWithoutZero, digit, otherChar, smallLowerCaseHexNumber, unicodeLowerCaseChar, oneOf("&", "|", "^", ":", "$", "(", ")", "{", "}", "=", "<", ">", "!", ",", "+", "*", "?", ".", "-"))

  def characterClassRange = rangeAtom.map(v => v + "-" + (v.charAt(0).toInt + 2).toString)

  def rangeAtom = oneOf(smallLowerCaseHexNumber, unicodeLowerCaseChar, octalNumberWithoutZero, digit, otherChar, oneOf("&", "|", "^", "-", ":", "$", "(", ")", "{", "}", "=", "<", ">", "!", ",", "+", "*", "?", "."))

  def nonCapturingGroup: Gen[String] = {
    for{
      withFlags <- genBool
      flags1    <- flags
      size      <- choose(0, 10)
      values    <- listOfN(1, regexAtom)
    } yield ( "(?" + (if (withFlags) flags1 else "") + ":" + values.mkString("")  + ")")
  }

  def atomicGroup = {
    for{
      size      <- choose(0, 10)
      values    <- listOfN(1, regexAtom)
    } yield ( "(?>" + values.mkString("")  + ")")
  }

  def namedCaptureGroup = {
    for{
      name      <- listOfN(5, alphaNumChar)
      size      <- choose(0, 10)
      values    <- listOfN(1, regexAtom)
    } yield ( "(?<" + name.mkString("") + ">" + values.mkString("")  + ")")
  }

  def lookAround = {
    for{
      ahead     <- genBool
      positive  <- genBool
      size      <- choose(0, 10)
      values    <- listOfN(1, regexAtom)
    } yield ( "(?" + (if (ahead) "" else "<") + (if (positive) "=" else "!") + values.mkString("")  + ")")
  }

  def group = {
    for{
      size      <- choose(0, 10)
      values    <- listOfN(1, regexAtom)
    } yield ( "(" + values.mkString("")  + ")") 
  }

  private def hexDigit   = frequency((1, choose(48, 57)), (1, choose(65, 70)), (1, choose(97, 102))).map(_.toChar)
}