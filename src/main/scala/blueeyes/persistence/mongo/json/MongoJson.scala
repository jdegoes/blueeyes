package blueeyes.persistence.mongo.json

import collection.mutable.ListBuffer
import blueeyes.json.JsonAST._
import com.mongodb.{BasicDBObject, DBObject}
import scala.collection.JavaConversions._

object MongoJson {
  trait CanConvertToJValue { def toJValue: JValue }
  
  implicit def string2JValue(v: String)             = new CanConvertToJValue { def toJValue: JString = JString(v) }
  implicit def long2JValue(v: java.lang.Long)       = new CanConvertToJValue { def toJValue: JInt    = JInt(v.longValue) }
  implicit def integer2JValue(v: java.lang.Integer) = new CanConvertToJValue { def toJValue: JInt    = JInt(v.intValue) }
  implicit def double2JValue(v: java.lang.Double)   = new CanConvertToJValue { def toJValue: JDouble = JDouble(v.doubleValue) }
  implicit def float2JValue(v: java.lang.Float)     = new CanConvertToJValue { def toJValue: JDouble = JDouble(v.floatValue.doubleValue) }
  implicit def boolean2JValue(v: java.lang.Boolean) = new CanConvertToJValue { def toJValue: JBool   = JBool(v.booleanValue) }
  implicit def arrayList2JValue(v: java.util.ArrayList[AnyRef]) = new CanConvertToJValue { def toJValue: JArray = JArray(v.map(anyRef2JValue).toList) }
  implicit def dbObject2JValue(v: DBObject) = new CanConvertToJValue { def toJValue: JObject = mongoObject2JObject(v) }

  implicit def mongoObject2JObject(dbObject: DBObject): JObject = {
    val allKeys  = asSet(dbObject.keySet)
    val pureKeys = allKeys.filter(_ != "_id")

    def toJField(key: String): JField = {
      JField(key, anyRef2JValue(dbObject.get(key)))
    } 

    JObject(pureKeys.foldLeft(List[JField]()){ (obj, key) => toJField(key) :: obj })
  }

  implicit def jObject2MongoObject(jObject: JObject): DBObject = {
    val dbObject = new BasicDBObject()

    jObject.fields.foreach(obj => {
      jValue2MongoValue(obj.value) match {
        case Some(v) => dbObject.put(obj.name, v)
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
    // Missing cases: com.mongodb.ObjectId, java.util.regex.Pattern, java.util.Date, com.mongodb.DBRef, byte[]
    case _                               => error("Unknown type for. {type=" + value.getClass  + "value=" + value + "}")
  }

  private def jValue2MongoValue[T <: JValue](value: T): Option[AnyRef]  = {
    import Predef._
    
    value match {
      case x: JString => Some(x.value)
      case x: JInt    => Some(long2Long(x.value.longValue))
      case x: JDouble => Some(double2Double(x.value))
      case x: JBool   => Some(boolean2Boolean(x.value))
      case x: JObject => Some(jObject2MongoObject(x))
      case x: JArray  => {
        val values = new java.util.ArrayList[AnyRef]
        x.elements.foreach(y => { 
          jValue2MongoValue(y) match { 
            case Some(z) => values.add(z)
            case _ => 
          }
        })
        Some(values)
      }
      case JNothing   => None
      case JNull      => Some(null)
      case _          => error("Unknown type for value: " + value)
    }
  }
}

