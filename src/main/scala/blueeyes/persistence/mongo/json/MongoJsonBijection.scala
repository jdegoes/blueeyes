package blueeyes.persistence.mongo.json

import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo.{MongoBijectionFactory, MongoBijection}
import org.joda.time.DateTime

object MongoJsonBijection extends MongoBijection[JValue, JField, JObject]{
  val factory = MongoJsonBijectionFactory

  object MongoJsonBijectionFactory extends MongoBijectionFactory[JValue, JField, JObject]{
    def mongoBinary(value: Array[Byte])                 = sys.error("Binary type is not supported")
    def mongoDate(value: DateTime)                      = sys.error("Date type is not supported")
    def mongoRegex(pattern: String, flags: Option[Int]) = sys.error("Regex type is not supported")
    def mongoString(value: String)                      = JString(value)
    def mongoLong(value: Long)                          = JInt(value)
    def mongoInteger(value: Int)                        = JInt(value)
    def mongoDouble(value: Double)                      = JDouble(value)
    def mongoFloat(value: Float)                        = JDouble(value.doubleValue)
    def mongoBoolean(value: Boolean)                    = JBool(value)
    def mongoArray(value: List[JValue])                 = JArray(value)
    def mongoNull                                       = JNull
    def mongoField(name: String, value: JValue)         = JField(name, value)
    def mongoObject(fields: List[JField])               = JObject(fields)
  }
}

