package blueeyes.core.service

sealed trait RangeUnit extends Product {
  def name: String = productPrefix
  def value: String = name
}

object RangeUnits {
  case object none extends RangeUnit
  case object bytes extends RangeUnit
}
