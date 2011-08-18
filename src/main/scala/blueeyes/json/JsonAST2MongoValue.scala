package blueeyes.json

object JsonAST2MongoValue{
  import JsonAST2._
  object MongoValueProvider {
    implicit val provider = new JsonProvider[MongoValue, MongoField]{
      val jArray   = MongoArray.apply _
      val jObject  = MongoObject.apply _
      val jNothing = () => MongoNothing
    }
  }

  sealed trait MongoValue extends JsonLike[MongoValue, MongoField]{
    val provider = MongoValueProvider.provider
  }

  case object MongoNothing extends JsonLikeNothing[MongoValue, MongoField] with MongoValue{
    type Self = MongoValue
  }

  case class MongoField(override val name: String, override val value: MongoValue) extends JsonLikeField[MongoValue, MongoField](name, value) with MongoValue {
    type Self = MongoField
  }

  case class MongoObject(override val fields: List[MongoField]) extends JsonLikeObject[MongoValue, MongoField](fields) with MongoValue {
    type Self = MongoObject

    override def equals(that: Any) = that match{
      case that: MongoObject => super.equals(that)
      case _ => false
    }
  }
  object MongoObject{
    lazy val empty = MongoObject(Nil)
  }

  case class MongoArray(override val elements: List[MongoValue]) extends JsonLikeArray[MongoValue, MongoField](elements) with MongoValue {
    type Self = MongoArray
  }
  object MongoArray {
    lazy val empty = MongoArray(Nil)
  }

  case class MongoString(value: String) extends MongoValue {
    type Values = String
    type Self = MongoValue

    def values = value
  }
}
