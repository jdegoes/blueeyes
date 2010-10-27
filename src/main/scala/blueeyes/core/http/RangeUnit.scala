package blueeyes.core.http
import blueeyes.util.ProductPrefixUnmangler

/* For use in the Accept-Ranges Http Header */

sealed trait RangeUnit extends ProductPrefixUnmangler{
  def value: String = unmangledName 
  
  override def toString = value
}

object RangeUnits {

  def parseRangeUnits(inString: String): Option[RangeUnit] = {
    def outRangeUnits: Option[RangeUnit] = ("""([a-z])+""").r.findFirstIn(inString.toLowerCase.trim)
      .getOrElse("bytes") match {
      case "none" => Some(none)
      case "bytes" => Some(bytes)
      case x => None
    }
    return outRangeUnits
  }

  case object none extends RangeUnit
  case object bytes extends RangeUnit

  sealed case class CustomToken(override val value: String) extends RangeUnit
}
