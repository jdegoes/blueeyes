package blueeyes.core.http

import scala.util.parsing.combinator._
import scala.util.parsing.input._
/* Language Ranges are used for the Accept-Language Http Header */

sealed trait LanguageRange {
  def mainType:   String
  def subType:    Option[String]
  def subSubType: Option[String]

  def value: String = (mainType :: Nil ++ subType.toList ++ subSubType.toList).mkString("-")
  
  override def toString = value
}

object LanguageRanges extends RegexParsers {
  private def elementParser = opt(
    regex("""(?:\b|^)""".r) ~> regex("""[a-z]{2}""".r) ~ ("-" ~>  regex("[a-z]+".r)) ~ ("-" ~>  regex("[a-z]+".r)) <~ regex("[^,]*".r) ^^ {case mainType ~ subType ~ subSubType => Range(mainType, subType, subSubType)} |
    regex("""(?:\b|^)""".r) ~> regex("""[a-z]{2}""".r) ~ ("-" ~>  regex("[a-z]+".r)) <~ regex("[^,]*".r) ^^ {case mainType ~ subType => Range(mainType, subType)} |
    regex("""(?:\b|^)""".r) ~> regex("""([a-z]{2})""".r) <~ regex("[^,]*".r) ^^ {case mainType => Range(mainType)})

  private def parser = repsep(elementParser, regex("""[ ]*,[ ]*""".r))

  def parseLanguageRanges(inString: String): List[Range] = parser(new CharSequenceReader(inString.toLowerCase)) match {
    case Success(result, _) => result filter(_ != None) map (_.get)

    case Failure(msg, _) => error("The LanguageRanges " + inString + " has a syntax error: " + msg)

    case Error(msg, _) => error("There was an error parsing \"" + inString + "\": " + msg)
  }

  case class Range (mainType: String, subType: Option[String], subSubType: Option[String]) extends LanguageRange
    
  object Range {
    def apply(mainType: String): Range = new Range(mainType, None, None)
    
    def apply(mainType: String, subType: String): Range = new Range(mainType, Some(subType), None)
    
    def apply(mainType: String, subType: String, subSubType: String): Range = new Range(mainType, Some(subType), Some(subSubType))
  }
}
