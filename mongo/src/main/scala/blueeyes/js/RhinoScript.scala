package blueeyes.js

import scala.collection.JavaConversions._
import blueeyes.json.JsonAST._
import org.mozilla.javascript.{ScriptableObject, Scriptable, Context, Undefined}

trait RhinoJsonImplicits{
  trait CanConvertToJValue { def toJValue: JValue }

  implicit def string2JValue(v: String)             = new CanConvertToJValue { def toJValue: JString = JString(v) }
  implicit def long2JValue(v: java.lang.Long)       = new CanConvertToJValue { def toJValue: JNum    = JNum(v.longValue) }
  implicit def integer2JValue(v: java.lang.Integer) = new CanConvertToJValue { def toJValue: JNum    = JNum(v.intValue) }
  implicit def double2JValue(v: java.lang.Double)   = new CanConvertToJValue { def toJValue: JValue = JNum.fromDouble(v.doubleValue) }
  implicit def float2JValue(v: java.lang.Float)     = new CanConvertToJValue { def toJValue: JValue = JNum.fromDouble(v.floatValue.doubleValue) }
  implicit def boolean2JValue(v: java.lang.Boolean) = new CanConvertToJValue { def toJValue: JBool   = JBool(v.booleanValue) }
  implicit def arrayList2JValue[T](v: java.util.ArrayList[T]) = new CanConvertToJValue { def toJValue: JArray = JArray(v.map(anyRef2JValue).toList) }
  implicit def dbObject2JValue(v: Scriptable) = new CanConvertToJValue { def toJValue: JObject = scriptableObject2JObject(v) }

  implicit def scriptableObject2JObject(dbObject: Scriptable): JObject = {
    val allKeys  = dbObject.getIds

    def toJField(key: String): JField = {
      JField(key, anyRef2JValue(dbObject.get(key, dbObject)))
    }
    JObject(allKeys.foldLeft(List[JField]()){ (obj, key) => toJField(key.asInstanceOf[String]) :: obj })
  }

  implicit def anyRef2JValue[T](value: T): JValue = value match {
    case x: String                  => x.toJValue
    case x: java.lang.Long          => x.toJValue
    case x: java.lang.Integer       => x.toJValue
    case x: java.lang.Double        => x.toJValue
    case x: java.lang.Float         => x.toJValue
    case x: java.lang.Boolean       => x.toJValue
    case x: java.util.ArrayList[_]  => x.toJValue
    case x: Scriptable              => x.toJValue
    case null                       => JNull
    case _                          => sys.error("Unknown type for. {value=" + value + "}")
  }
}

object RhinoJsonImplicits extends RhinoJsonImplicits

case class RhinoScript(script: String) extends RhinoJsonImplicits{
  def apply(scopedObjects: Map[String, _] = Map()): Option[JValue] = {
    val context = Context.enter();
    try{
      val scope = context.initStandardObjects()

      scopedObjects.foreach(scopedObject => ScriptableObject.putProperty(scope, scopedObject._1, scopedObject._2))

      val result = context.evaluateString(scope, script, "javascript.log", 1, null)
      Option(result).flatMap{_ match {
        case e: Undefined => None
        case e => Some(anyRef2JValue(e))
      }}

    }
    finally{
      Context.exit()
    }
  }
}
