package blueeyes.js

import scala.collection.JavaConversions._
import blueeyes.json.JsonAST._
import org.mozilla.javascript.{ScriptableObject, Scriptable, Context}

trait RhinoJsonImplicits{
  trait CanConvertToJValue { def toJValue: JValue }

  implicit def string2JValue(v: String)             = new CanConvertToJValue { def toJValue: JString = JString(v) }
  implicit def long2JValue(v: java.lang.Long)       = new CanConvertToJValue { def toJValue: JInt    = JInt(v.longValue) }
  implicit def integer2JValue(v: java.lang.Integer) = new CanConvertToJValue { def toJValue: JInt    = JInt(v.intValue) }
  implicit def double2JValue(v: java.lang.Double)   = new CanConvertToJValue { def toJValue: JDouble = JDouble(v.doubleValue) }
  implicit def float2JValue(v: java.lang.Float)     = new CanConvertToJValue { def toJValue: JDouble = JDouble(v.floatValue.doubleValue) }
  implicit def boolean2JValue(v: java.lang.Boolean) = new CanConvertToJValue { def toJValue: JBool   = JBool(v.booleanValue) }
  implicit def arrayList2JValue(v: java.util.ArrayList[AnyRef]) = new CanConvertToJValue { def toJValue: JArray = JArray(v.map(anyRef2JValue).toList) }
  implicit def dbObject2JValue(v: Scriptable) = new CanConvertToJValue { def toJValue: JObject = scriptableObject2JObject(v) }

  implicit def scriptableObject2JObject(dbObject: Scriptable): JObject = {
    val allKeys  = dbObject.getIds

    def toJField(key: String): JField = {
      JField(key, anyRef2JValue(dbObject.get(key, dbObject)))
    }

    JObject(allKeys.foldLeft(List[JField]()){ (obj, key) => toJField(key.asInstanceOf[String]) :: obj })
  }

  private def anyRef2JValue(value: AnyRef): JValue = value match {
    case x: String                       => x.toJValue
    case x: java.lang.Long               => x.toJValue
    case x: java.lang.Integer            => x.toJValue
    case x: java.lang.Double             => x.toJValue
    case x: java.lang.Float              => x.toJValue
    case x: java.lang.Boolean            => x.toJValue
    case x: java.util.ArrayList[AnyRef]  => x.toJValue
    case x: Scriptable                     => x.toJValue
    case null                            => JNull
    case _                               => error("Unknown type for. {type=" + value.getClass  + "value=" + value + "}")
  }
}

object RhinoJsonImplicits extends RhinoJsonImplicits

case class RhinoScript(script: String) extends RhinoJsonImplicits{
  def apply(scopedObjects: Map[String, _] = Map()): Option[JObject] = {
    val context = Context.enter();
    try{
      val scope = context.initStandardObjects();

      scopedObjects.foreach(scopedObject => ScriptableObject.putProperty(scope, scopedObject._1, scopedObject._2))

      context.evaluateString(scope, script, "javascript.log", 1, null) match {
        case e: Scriptable => Some(e)
        case _             => None
      }
    }
    finally{
      Context.exit();
    }
  }
}
