package blueeyes.core.storeable

sealed trait Storeable

trait Record extends Storeable
trait Value extends Storeable

trait ValueImplicits{
  implicit def intToValue(value: Int)         = ValueInt(value)
  implicit def bigIntToValue(value: BigInt)   = ValueBigInt(value)
  implicit def booleanToValue(value: Boolean) = ValueBoolean(value)
  implicit def doubleToValue(value: Double)   = ValueDouble(value)
  implicit def stringToValue(value: String)   = ValueString(value)
  implicit def charToValue(value: Char)       = ValueChar(value)
  implicit def byteToValue(value: Byte)       = ValueByte(value)
  implicit def floatToValue(value: Float)     = ValueFloat(value)
  implicit def longToValue(value: Long)       = ValueLong(value)
  implicit def shortToValue(value: Short)     = ValueShort(value)

  implicit def optionToValue[T](value: Option[T])(implicit valueToStoreable: T => Storeable)  = ValueOption(value)
  implicit def seqToValue[T](value: Seq[T])(implicit valueToStoreable: T => Storeable)        = ValueSeq(value)
  implicit def setToValue[T](value: Set[T])(implicit valueToStoreable: T => Storeable)        = ValueSet(value)
  implicit def mapToValue[K, V](value: Map[K, V])(implicit keyToStoreable: K => Storeable, valueToStoreable: V => Storeable)     = ValueMap(value)
}

object ValueImplicits extends ValueImplicits