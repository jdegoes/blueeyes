package blueeyes.json

import blueeyes.json.JsonAST2._

class JsonJValue extends Json[JValue]{
  def fold[Z](value: JValue, prim: (JValue) => Z, arr: (Iterable[JValue]) => Z, obj: (Iterable[(String, JValue)]) => Z): Z = value match{
    case JArray(v)    => arr(v)
    case JObject(v)   => obj(v)
    case _            => prim(value)
  }

  def toArray(i: Iterable[JValue]) = JArray(i.toList)

  def toObject(i: Iterable[(String, JValue)]) = JObject(Map(i.toList: _*))
}

object JsonAST2{
  sealed trait JValue extends Product

  case object JNull extends JValue

  case class JBool(value: Boolean) extends JValue

  case class JInt(value: BigInt) extends JValue

  case class JDouble(value: Double) extends JValue

  case class JString(value: String) extends JValue

  case class JObject(fields: Map[String, JValue]) extends JValue
  object JObject {
    lazy val empty = JObject(Map())
  }

  case class JArray(elements: List[JValue]) extends JValue
  object JArray {
    lazy val empty = JArray(Nil)
  }

}

//
//  val zero: JValue = JNothing
//
//  def fold[Z](nil: => Z, value: JValue, prim: (JValue) => Z, field: (String, JValue) => Z, arr: (List[JValue]) => Z, obj: (List[JField]) => Z) = value match {
//    case JField(n, v) => field(n, v)
//    case JArray(v)    => arr(v)
//    case JObject(v)   => obj(v)
//    case _            => prim(value)
//  }
//
//  def jsonArray(elements: List[JValue])      = JArray(elements)
//  def jsonObject(elements: List[JField])     = JObject(elements)
//}
//
//trait JsonLikeJValueImplicits extends ToRichJsonLike{
//  implicit val jsonLikeJValue = new JsonLikeJValue()
//  implicit def jValueToRichJsonLike[T <: JValue](value: T) = toRichJson[JValue, JField](value)
//}
//object JsonLikeJValueImplicits extends JsonLikeJValueImplicits
//
//object JsonLikeTest extends JsonLikeJValueImplicits with ToRichJsonLike{
//  def main(args: Array[String]){
//    val value = JObject(JField("bar", JString("dd")) :: Nil)
//
//    println(value.\("bar"))
//    println(value.+\("bar"))
//  }
//}