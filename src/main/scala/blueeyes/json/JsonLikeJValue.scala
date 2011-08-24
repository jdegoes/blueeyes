package blueeyes.json

import blueeyes.json.JsonAST._

class JsonLikeJValue extends JsonLike[JValue, JField]{
  def unfold(value: JValue) = value match{
    case JObject(fields)  => Some(Left(fields))
    case JArray(elements) => Some(Right(elements))
    case _                => None
  }

  def unfoldOne(value: JValue) = value match{
    case JField(n, v) => v
    case _ => value
  }

  def foldOne(name: String, value: JValue) = JField(name, value)

  def children(value: JValue): List[JValue] = value match {
    case JObject(l)   => l
    case JArray(l)    => l
    case JField(n, v) => List(v)
    case _ => Nil
  }

  def foldArr(elemets: List[JValue]) = JArray(elemets)

  def foldObj(elemets: List[JField]) = JObject(elemets)

  val zero: JValue = JNothing

  def name(value: JValue) = value match{
    case JField(n, v) => Some(n)
    case _ => None
  }
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