package blueeyes.core.service

sealed trait RangeUnit extends Product {
  def value: String = productPrefix
  
  override def toString = value
}

object RangeUnits {
  case object none extends RangeUnit
  case object bytes extends RangeUnit
}
