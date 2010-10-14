package blueeyes.persistence.mongo.json

import collection.mutable.ListBuffer
import blueeyes.json.JsonAST._
import com.mongodb.{BasicDBObject, DBObject}
import scala.collection.JavaConversions._

object MongoJson {
  case class StringToJValue(v: String) { def toJValue: JString = JString(v) }
  case class LongToJValue(v: java.lang.Long) { def toJValue: JInt = JInt(v.longValue) }
  case class IntegerToJValue(v: java.lang.Integer) { def toJValue: JInt = JInt(v.intValue) }
  case class DoubleToJValue(v: java.lang.Double) { def toJValue: JDouble = JDouble(v.doubleValue) }
  case class FloatToJValue(v: java.lang.Float) { def toJValue: JDouble = JDouble(v.floatValue.doubleValue) }
  case class BooleanToJValue(v: java.lang.Boolean) { def toJValue: JBool = JBool(v.booleanValue) }
  case class ArrayListToJValue(v: java.util.ArrayList[AnyRef]) { 
    def toJValue: JArray = {
      val values   = new ListBuffer[JValue]()
      val iterator = v.iterator
      while (iterator.hasNext){
        values += anyRef2JValue(iterator.next)
      }

      JArray(values.toList)
    }
  }
  case class DBObjectToJValue(v: DBObject) { def toJValue: JObject = mongoObject2JObject(v) }

  implicit def string2JValue(v: String): StringToJValue = StringToJValue(v)
  implicit def long2JValue(v: java.lang.Long): LongToJValue = LongToJValue(v)
  implicit def integer2JValue(v: java.lang.Integer): IntegerToJValue = IntegerToJValue(v)
  implicit def double2JValue(v: java.lang.Double): DoubleToJValue = DoubleToJValue(v)
  implicit def float2JValue(v: java.lang.Float): FloatToJValue = FloatToJValue(v)
  implicit def boolean2JValue(v: java.lang.Boolean): BooleanToJValue = BooleanToJValue(v)
  implicit def arrayList2JValue(v: java.util.ArrayList[AnyRef]): ArrayListToJValue = ArrayListToJValue(v)
  implicit def dbObject2JValue(v: DBObject): DBObjectToJValue = DBObjectToJValue(v)


  implicit def mongoObject2JObject(dbObject: DBObject): JObject = {
    val allKeys   = asSet(dbObject.keySet)
    val pureKeys  = allKeys.filter(!_.startsWith("_"))

    def toJField(key: String): JField = {
      JField(key, anyRef2JValue(dbObject.get(key)))
    } 

    JObject(pureKeys.foldLeft(List[JField]()){ (obj, key) => toJField(key) :: obj })
  }

  implicit def jObject2MongoObject(jObject: JObject): DBObject = {
    val dbObject = new BasicDBObject()

    jObject.obj.foreach(obj => {
      jValue2MongoValue(obj.value) match {
        case Some(v)    => dbObject.put(obj.name, v)
        case None =>
      }
    })

    dbObject
  }

  private def anyRef2JValue(value: AnyRef): JValue = value match {
    case x: String                       => x.toJValue
    case x: java.lang.Long               => x.toJValue
    case x: java.lang.Integer            => x.toJValue
    case x: java.lang.Double             => x.toJValue
    case x: java.lang.Float              => x.toJValue
    case x: java.lang.Boolean            => x.toJValue
    case x: java.util.ArrayList[AnyRef]  => x.toJValue
    case x: DBObject                     => x.toJValue
    case null                            => JNull
    case _                               => error("Unknown type for. {type=" + value.getClass  + "value=" + value + "}")
  }

  import Predef._
  private def jValue2MongoValue[T <: JValue](value: T): Option[AnyRef]  = value match {
    case x: JString => Some(new String(x.s))
    case x: JInt    => Some(long2Long(x.num.longValue))
    case x: JDouble => Some(double2Double(x.num))
    case x: JBool   => Some(boolean2Boolean(x.value))
    case x: JObject => Some(jObject2MongoObject(x))
    case x: JArray  => {
      if (!x.arr.isEmpty){
        val values = new java.util.ArrayList[AnyRef]
        x.arr.foreach(y => { 
          jValue2MongoValue(y) match { 
            case Some(z) => values.add(z)
            case _ => 
          }
        })
        Some(values)
      }
      else None
    }
    case JNothing   => None
    case JNull      => Some(null)
    case _          => error("Unknown type for value: " + value)
  }
}

