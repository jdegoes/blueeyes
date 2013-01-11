package blueeyes.persistence.mongo

import blueeyes.json._

sealed trait MongoPrimitive {
  def toJValue: JValue
}

case class MongoPrimitiveOption(value: Option[MongoPrimitive]) extends MongoPrimitive {
  def toJValue = value.map(_.toJValue).getOrElse(JUndefined)
}
case class MongoPrimitiveString(value: String) extends MongoPrimitive {
  def toJValue = JString(value)
}
case class MongoPrimitiveLong(value: Long) extends MongoPrimitive {
  def toJValue = JNum(value)
}
case class MongoPrimitiveInt(value: Int) extends MongoPrimitive {
  def toJValue = JNum(value)
}
case class MongoPrimitiveBigInt(value: BigInt) extends MongoPrimitive {
  def toJValue = JNum(BigDecimal(value))
}
case class MongoPrimitiveDouble(value: Double) extends MongoPrimitive {
  def toJValue = JNum(value)
}
case class MongoPrimitiveBoolean(value: Boolean) extends MongoPrimitive {
  def toJValue = JBool(value)
}
case class MongoPrimitiveArray(value: List[MongoPrimitive]) extends MongoPrimitive {
  def toJValue = JArray(value.map(_.toJValue))
}
case class MongoPrimitiveJObject(value: JObject) extends MongoPrimitive {
  def toJValue = value
}
case object MongoPrimitiveNull extends MongoPrimitive {
  def toJValue = JNull
}

sealed class MongoPrimitiveWitness[T](val typeNumber: Int)

object MongoPrimitiveWitness {
  /*
  Double	 1
  String	 2
  Object	 3
  Array	 4
  Binary data	 5
  Object id	 7
  Boolean	 8
  Date	 9
  Null	 10
  Regular expression	 11
  JavaScript code	 13
  Symbol	 14
  JavaScript code with scope	 15
  32-bit integer	 16
  Timestamp	 17
  64-bit integer	 18
  Min key	 255
  Max key	 127
  */
  implicit val MongoPrimitiveJStringWitness = new MongoPrimitiveWitness[JString](2)
  implicit val MongoPrimitiveJObjectWitness = new MongoPrimitiveWitness[JObject](3)
  implicit val MongoPrimitiveJArrayWitness  = new MongoPrimitiveWitness[JArray](4)
  implicit val MongoPrimitiveJBoolWitness   = new MongoPrimitiveWitness[JBool](8)
  implicit val MongoPrimitiveJNullWitness   = new MongoPrimitiveWitness[JNull.type](10)
  implicit val MongoPrimitiveJNumWitness    = new MongoPrimitiveWitness[JNum](18)
}


// vim: set ts=4 sw=4 et:
