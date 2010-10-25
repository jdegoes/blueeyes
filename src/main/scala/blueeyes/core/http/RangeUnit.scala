package blueeyes.core.http
import blueeyes.util.ProductPrefixUnmangler

/* For use in the Accept-Ranges Http Header */

sealed trait RangeUnit extends ProductPrefixUnmangler{
  def value: String = productPrefix 
  
  override def toString = value
}

object RangeUnits {

  def parseRangeUnits(inString: String): RangeUnit = {
    def outRangeUnits: RangeUnit = ("""[a-z]+""").r.findFirstIn(inString.toLowerCase.trim)
      .getOrElse("bytes") match {
      case "none" => none
      case "bytes" => bytes
      case x => CustomToken(x)
    }
    return outRangeUnits
  }

  case object none extends RangeUnit
  case object bytes extends RangeUnit

  sealed case class CustomToken(override val value: String) extends RangeUnit
}
