package blueeyes.json

import blueeyes.json.JsonAST._

class JsonLikeJValue extends JsonLike[JValue, JField]{

  val zero: JValue = JNothing

  def fold[Z](value: JValue, prim: (JValue) => Z, field: (String, JValue) => Z, arr: (List[JValue]) => Z, obj: (List[JField]) => Z) = value match {
    case JField(n, v) => field(n, v)
    case JArray(v)    => arr(v)
    case JObject(v)   => obj(v)
    case _            => prim(value)
  }

  def jsonField(name: String, value: JValue) = JField(name, value)
  def jsonArray(elements: List[JValue])      = JArray(elements)
  def jsonObject(elements: List[JField])     = JObject(elements)
}

trait JsonLikeJValueImplicits extends ToRichJsonLike{
  implicit val jsonLikeJValue = new JsonLikeJValue()
  implicit def jValueToRichJsonLike[T <: JValue](value: T) = toRichJson[JValue, JField](value)
}
object JsonLikeJValueImplicits extends JsonLikeJValueImplicits

object JsonLikeTest extends JsonLikeJValueImplicits with ToRichJsonLike{
  def main(args: Array[String]){
    val value = JObject(JField("bar", JString("dd")) :: Nil)

    println(value.\("bar"))
    println(value.+\("bar"))
  }
}