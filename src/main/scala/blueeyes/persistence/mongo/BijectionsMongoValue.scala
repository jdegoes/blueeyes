package blueeyes.persistence.mongo

import org.joda.time.DateTime

trait BijectionsMongoValue {
  implicit val MongoToMongoValue = new MongoBijection[MongoValue, MongoField, MongoObject]{val factory = MongoValueBijectionFactory}
  implicit val MongoValueToMongo = MongoToMongoValue.inverse
}

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

object BijectionsMongoValue extends BijectionsMongoValue
