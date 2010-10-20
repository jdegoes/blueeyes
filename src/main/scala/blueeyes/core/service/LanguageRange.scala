package blueeyes.core.service

/* Language Ranges are used for the Accept-Language Http Header */

sealed trait LanguageRange {

  def mainType: String
  def subType: String
  def subSubType: String

  def value: String = {
    var out = mainType
    if (subType != "") {
      out += "-" + subType
    }
    if (subSubType != "") {
      out += "-" + subSubType
    }
    out
  }
}

object LanguageRanges {
  case class Range (mainType: String, subType: String, subSubType: String) extends LanguageRange {
    def this(mainType: String) = this(mainType, "", "");
    def this(mainType: String, subType: String) = this(mainType, subType, "")
  }
}
