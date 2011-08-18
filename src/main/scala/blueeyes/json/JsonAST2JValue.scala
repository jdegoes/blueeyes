package blueeyes.json

object JsonAST2JValue{
  import JsonAST2._
  object JValueProvider{
    implicit val provider = new JsonProvider[JValue, JField]{
      val jArray   = JArray.apply _
      val jObject  = JObject.apply _
      val jNothing = () => JNothing
    }
  }

  sealed trait JValue extends JsonLike[JValue, JField]{
    val provider = JValueProvider.provider
  }

  case object JNothing extends JsonLikeNothing[JValue, JField] with JValue{
    type Self = JValue
  }

  case class JField(override val name: String, override val value: JValue) extends JsonLikeField[JValue, JField](name, value) with JValue {
    type Self = JField
  }

  case class JObject(override val fields: List[JField]) extends JsonLikeObject[JValue, JField](fields) with JValue {
    type Self = JObject

    override def equals(that: Any) = that match{
      case that: JObject => super.equals(that)
      case _ => false
    }
  }
  object JObject{
    lazy val empty = JObject(Nil)
  }

  case class JArray(override val elements: List[JValue]) extends JsonLikeArray[JValue, JField](elements) with JValue {
    type Self = JArray
  }
  object JArray {
    lazy val empty = JArray(Nil)
  }

  case class JString(value: String) extends JValue {
    type Values = String
    type Self = JValue

    def values = value
  }
}