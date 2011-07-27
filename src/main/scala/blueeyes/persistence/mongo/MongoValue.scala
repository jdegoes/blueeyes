package blueeyes.persistence.mongo

import org.joda.time.DateTime
import java.util.regex.Pattern

sealed trait MongoValue{
  type Values;
  def values: Values
}

case class MongoBoolean(value: Boolean) extends MongoValue{
  type Values = Boolean
  def values = value
}

case class MongoInt(value: Int) extends MongoValue{
  type Values = Int
  def values = value
}

case class MongoLong(value: Long) extends MongoValue{
  type Values = Long
  def values = value
}

case class MongoDouble(value: Double) extends MongoValue{
  type Values = Double
  def values = value
}

case class MongoFloat(value: Float) extends MongoValue{
  type Values = Float
  def values = value
}

case class MongoString(value: String) extends MongoValue{
  type Values = String
  def values = value
}

case class MongoRegex(pattern: String, options: Option[Int]) extends MongoValue{
  type Values = (String, Int)
  def values = (pattern, options.getOrElse(0))
}

case class MongoBinary(value: Array[Byte]) extends MongoValue{
  type Values = Array[Byte]
  def values  = value
}

case class MongoDate(value: DateTime) extends MongoValue{
  type Values = DateTime
  def values = value
}

case object MongoNull extends MongoValue{
  type Values = Null
  def values = null
}

case class MongoField(name: String, value: MongoValue) extends MongoValue{
  type Values = (String, value.Values)
  def values = (name, value.values)
}

case class MongoArray(elements: List[MongoValue]) extends MongoValue{
  type Values = List[Any]
  def values = elements.map(_.values)
}

case class MongoObject(fields: List[MongoField]) extends MongoValue{
  type Values = Map[String, Any]
  def values = Map() ++ fields.map(_.values : (String, Any))

  override lazy val hashCode = Set(this.fields: _*).hashCode

  override def equals(that: Any): Boolean = that match {
    case that: MongoObject if (this.fields.length == that.fields.length) => Set(this.fields: _*) == Set(that.fields: _*)
    case _ => false
  }
}

object MongoValueBijection{
  implicit val converter = new MongoBijection[MongoValue, MongoField, MongoObject]{val factory = MongoValueBijectionFactory}

  object MongoValueBijectionFactory extends MongoBijectionFactory[MongoValue, MongoField, MongoObject]{
    def mongoBinary(value: Array[Byte])                 = MongoBinary(value)
    def mongoDate(value: DateTime)                      = MongoDate(value)
    def mongoRegex(pattern: String, flags: Option[Int]) = MongoRegex(pattern, flags)
    def mongoString(value: String)                      = MongoString(value)
    def mongoLong(value: Long)                          = MongoLong(value)
    def mongoInteger(value: Int)                        = MongoInt(value)
    def mongoDouble(value: Double)                      = MongoDouble(value)
    def mongoFloat(value: Float)                        = MongoFloat(value)
    def mongoBoolean(value: Boolean)                    = MongoBoolean(value)
    def mongoArray(value: List[MongoValue])             = MongoArray(value)
    def mongoNull                                       = MongoNull
    def mongoField(name: String, value: MongoValue)     = MongoField(name, value)
    def mongoObject(fields: List[MongoField])           = MongoObject(fields)
  }
}
