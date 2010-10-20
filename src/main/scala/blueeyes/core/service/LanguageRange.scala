package blueeyes.core.service

/* Language Ranges are used for the Accept-Language Http Header */

sealed trait LanguageRange {
  def mainType:   String
  def subType:    Option[String]
  def subSubType: Option[String]

  def value: String = (mainType :: Nil ++ subType.toList ++ subSubType.toList).mkString("-")
  
  override def toString = value
}

object LanguageRanges {
  case class Range (mainType: String, subType: Option[String], subSubType: Option[String]) extends LanguageRange
    
  object Range {
    def apply(mainType: String): Range = new Range(mainType, None, None)
    
    def apply(mainType: String, subType: String): Range = new Range(mainType, Some(subType), None)
    
    def apply(mainType: String, subType: String, subSubType: String): Range = new Range(mainType, Some(subType), Some(subSubType))
  }
}
