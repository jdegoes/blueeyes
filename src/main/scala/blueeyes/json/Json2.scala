package blueeyes.json2

object JsonAST2{
  trait JsonProvider[A <: JsonLike[_]]{
    def jArray: List[A] => A

    def jNothing: () => A
  }

  trait JsonLike[A <: JsonLike[_]] {

    def provider: JsonProvider[A]

    type Values
    type Self <: A

    /** XPath-like expression to query JSON fields by name. Matches only fields on
     * next level.
     * <p>
     * Example:<pre>
     * json \ "name"
     * </pre>
     */
    def \ (nameToFind: String): A = {
      def extractValue(jvalue: A): A = jvalue match {
        case JsonLikeField(n, v) => v
        case _ => jvalue
      }
      val p = (json: A) => json match {
        case JsonLikeField(name, value) if name == nameToFind => true
        case _ => false
      }
      findDirect(children, p) match {
        case Nil => provider.jNothing()
        case x :: Nil => extractValue(x)
        case xs => provider.jArray(xs.map(extractValue))
      }
    }

      /** Return direct child elements.
       * <p>
       * Example:<pre>
       * JArray(JInt(1) :: JInt(2) :: Nil).children == List(JInt(1), JInt(2))
       * </pre>
       */
      def children: List[A] = this match {
        case e: JsonLikeObject[A, A] => e.fields
        case JsonLikeArray(l) => l
        case e: JsonLikeField[A] => List(e.value)
        case _ => Nil
      }

    private def findDirect(xs: List[A], p: A => Boolean): List[A] = xs.flatMap {
      case JsonLikeObject(l) => l.filter {
        case x if p(x) => true
        case _ => false
      }
      case e: JsonLikeArray[A] => findDirect(e.elements, p)
      case x if p(x) => x :: Nil
      case _ => Nil
    }

    /** Return unboxed values from JSON
     * <p>
     * Example:<pre>
     * JObject(JField("name", JString("joe")) :: Nil).values == Map("name" -> "joe")
     * </pre>
     */
    def values: Values

    def apply(i: Int): A = provider.jNothing()
  }

  abstract class JsonLikeField[A <: JsonLike[A]](val name: String, val value: A) extends JsonLike[A]{
    type Values = (String, value.Values)

    def values = (name, value.values)
    override def apply(i: Int): A = value.apply(i)
  }
  object JsonLikeField{
    def unapply[A <: JsonLike[_]](json: A): Option[(String, A)] = json match{
      case e: JsonLikeField[A] => Some((e.name, e.value))
      case _ => None
    }
  }

  abstract class JsonLikeObject[A <: JsonLike[A], B <: JsonLikeField[A]](val fields: List[B]) extends JsonLike[A]{
    type Values = Map[String, Any]

    def values = Map() ++ fields.map(_.values : (String, Any))

    override lazy val hashCode = Set(this.fields: _*).hashCode

    override def equals(that: Any): Boolean = that match {
      case that: JsonLikeObject[A, B] if (this.fields.length == that.fields.length) => Set(this.fields: _*) == Set(that.fields: _*)
      case _ => false
    }
  }
  object JsonLikeObject{
    def unapply[A <: JsonLike[_]](json: A): Option[List[A]] = json match{
      case e: JsonLikeObject[A, A] => Some(e.fields)
      case _ => None
    }
  }

  abstract class JsonLikeArray[A <: JsonLike[_]](val elements: List[A]) extends JsonLike[A]{
    type Values = List[Any]

    def values = elements.map(_.values)

    override def apply(i: Int): A = elements.lift(i).getOrElse(provider.jNothing())
  }
  object JsonLikeArray{
    def unapply[A <: JsonLike[_]](json: JsonLike[A]): Option[List[A]] = json match{
      case e: JsonLikeArray[A] => Some(e.elements)
      case _ => None
    }
  }
}

object JsonAST2JValue{
  import JsonAST2._
  object JValueProvider extends JsonProvider[JValue]{
    lazy val jArray = JArray.apply _

    lazy val jNothing = () => JNothing
  }

  sealed trait JValue extends JsonLike[JValue]{
    def provider = JValueProvider
  }

  case object JNothing extends JValue{
    type Self = JValue
    type Values = None.type

    def values = None
  }

  case class JField(override val name: String, override val value: JValue) extends JsonLikeField[JValue](name, value) with JValue {
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

  case class JArray(override val elements: List[JValue]) extends JsonLikeArray[JValue](elements) with JValue {
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

object JsonAST2MongoValue{
  import JsonAST2._
  object MongoValueProvider extends JsonProvider[MongoValue]{
    lazy val jArray = MongoArray.apply _

    lazy val jNothing = () => MongoNothing
  }

  sealed trait MongoValue extends JsonLike[MongoValue]{
    def provider = MongoValueProvider
  }

  case object MongoNothing extends MongoValue{
    type Self = MongoValue
    type Values = None.type

    def values = None
  }

  case class MongoField(override val name: String, override val value: MongoValue) extends JsonLikeField[MongoValue](name, value) with MongoValue {
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

  case class MongoArray(override val elements: List[MongoValue]) extends JsonLikeArray[MongoValue](elements) with MongoValue {
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

object TestJsonLike{
  import JsonAST2MongoValue._
  import JsonAST2JValue._
  def main(args: Array[String]){
    val jobject = JObject(JField("foo", JString("bar")) :: Nil)

    jobject \ "foo" match{
      case JString(value) => println(value)
      case _ => println("not found")
    }
    val mongoobject = MongoObject(MongoField("foo", MongoString("bar")) :: Nil)

    mongoobject \ "foo" match{
      case MongoString(value) => println(value)
      case _ => println("not found")
    }
  }
}