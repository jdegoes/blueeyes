package blueeyes.core.http

import scala.util.matching.Regex
/* Language Ranges are used for the Accept-Language Http Header */

sealed trait LanguageRange {
  def mainType:   String
  def subType:    Option[String]
  def subSubType: Option[String]

  def value: String = (mainType :: Nil ++ subType.toList ++ subSubType.toList).mkString("-")
  
  override def toString = value
}

object LanguageRanges {
  def parseLanguageRanges(inString: String): Array[LanguageRange] = {
    def ParseLanguageRegex = new Regex("""(?:\b|^)([a-z]{2}(\-[a-z]+){0,2})""")

    var outLangRanges: Array[LanguageRange] = inString.toLowerCase.split(",").map(_.trim)
    .flatMap(ParseLanguageRegex.findFirstIn(_)).map(_.split("-")).map { languageRanges =>
      languageRanges match {
        case Array(mainType, subType, subSubType) => Range(mainType, subType, subSubType)
        
        case Array(mainType, subType) => Range(mainType, subType)
        
        case Array(mainType) => Range(mainType)
      }
    }
    return outLangRanges
  }

  case class Range (mainType: String, subType: Option[String], subSubType: Option[String]) extends LanguageRange
    
  object Range {
    def apply(mainType: String): Range = new Range(mainType, None, None)
    
    def apply(mainType: String, subType: String): Range = new Range(mainType, Some(subType), None)
    
    def apply(mainType: String, subType: String, subSubType: String): Range = new Range(mainType, Some(subType), Some(subSubType))
  }
}
