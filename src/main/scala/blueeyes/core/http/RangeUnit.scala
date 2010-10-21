package blueeyes.core.http
import blueeyes.util.ProductPrefixUnmangler

/* For use in the Accept-Ranges Http Header */

sealed trait RangeUnit extends ProductPrefixUnmangler{
  def value: String = productPrefix 
  
  override def toString = value
}

object RangeUnits {
  case object none extends RangeUnit
  case object bytes extends RangeUnit

  sealed case class CustomToken(override val value: String) extends RangeUnit
}
