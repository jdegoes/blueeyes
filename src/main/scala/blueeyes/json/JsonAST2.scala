package blueeyes.json

object JsonAST2{

  /** Concatenates a sequence of <code>JValue</code>s.
   * <p>
   * Example:<pre>
   * concat(JInt(1), JInt(2)) == JArray(List(JInt(1), JInt(2)))
   * </pre>
   */
//  def concat[A <: JsonLike[A] : JsonProvider](xs: A*) = xs.foldLeft(implicitly[JsonProvider[A]].jNothing(): A)(_ ++ _)

  trait JsonProvider[A <: JsonLike[A, B], B <: JsonLikeField[A, B] with A]{
    def jArray: List[A]  => A
    def jNothing: ()     => A
    def jObject: List[B] => A
  }

  trait JsonLike[A <: JsonLike[A, B], B <: JsonLikeField[A, B] with A] {

    def provider: JsonProvider[A, B]

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
        case e: JsonLikeField[A, B] => e.value
        case _ => jvalue
      }
      val p = (json: A) => json match {
        case e: JsonLikeField[A, B] if e.name == nameToFind => true
        case _ => false
      }
      findDirect(children, p) match {
        case Nil => provider.jNothing()
        case x :: Nil => extractValue(x)
        case xs => provider.jArray(xs.map(extractValue))
      }
    }

    /**
     * Returns the element as a JValue of the specified class.
     * <p>
     * Example:<pre>
     * (json \ "foo" --> classOf[JField]).value
     * </pre>
     */
    def --> [C <: A](clazz: Class[C]): C = (this -->? clazz).getOrElse(sys.error("Expected class " + clazz + ", but found: " + this.getClass))

    /**
     * Returns the element as an option of a JValue of the specified class.
      * <p>
      * Example:<pre>
      * (json \ "foo" -->? classOf[JField]).map(_.value).getOrElse(defaultFieldValue)
      * </pre>
     */
    def -->? [C <: A](clazz: Class[C]): Option[C] = {
      def extractTyped(value: A) = if (value.getClass == clazz) Some(value.asInstanceOf[C]) else None

      this match {
        case e: JsonLikeField[A, B] if (!clazz.isInstanceOf[JsonLikeField[A, B]]) => extractTyped(e.value)
        case _ => extractTyped(selfTyped)
      }
    }

    /** Return direct child elements.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: Nil).children == List(JInt(1), JInt(2))
     * </pre>
     */
    def children: List[A] = this match {
      case e: JsonLikeObject[A, B] => e.fields
      case e: JsonLikeArray[A, B] => e.elements
      case e: JsonLikeField[A, B] => List(e.value)
      case _ => Nil
    }

    private def findDirect(xs: List[A], p: A => Boolean): List[A] = xs.flatMap {
      case e: JsonLikeObject[A, B] => e.fields.filter {
        case x if p(x) => true
        case _ => false
      }
      case e: JsonLikeArray[A, B] => findDirect(e.elements, p)
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

    private def selfTyped = this.asInstanceOf[A]
  }

  abstract class JsonLikeNothing[A <: JsonLike[A, B], B <: JsonLikeField[A, B] with A] extends JsonLike[A, B]{
    type Values = None.type

    def values = None
  }

  abstract class JsonLikeField[A <: JsonLike[A, B], B <: JsonLikeField[A, B] with A](val name: String, val value: A) extends JsonLike[A, B]{
    type Values = (String, value.Values)

    def values = (name, value.values)
    override def apply(i: Int): A = value.apply(i)
  }

  abstract class JsonLikeObject[A <: JsonLike[A, B], B <: JsonLikeField[A, B] with A](val fields: List[B]) extends JsonLike[A, B]{
    type Values = Map[String, Any]

    def values = Map() ++ fields.map(_.values : (String, Any))

    override lazy val hashCode = Set(this.fields: _*).hashCode

    override def equals(that: Any): Boolean = that match {
      case that: JsonLikeObject[A, B] if (this.fields.length == that.fields.length) => Set(this.fields: _*) == Set(that.fields: _*)
      case _ => false
    }
  }

  abstract class JsonLikeArray[A <: JsonLike[A, B], B <: JsonLikeField[A, B] with A](val elements: List[A]) extends JsonLike[A, B]{
    type Values = List[Any]

    def values = elements.map(_.values)

    override def apply(i: Int): A = elements.lift(i).getOrElse(provider.jNothing())
  }
}

object TestJsonLike{
  import JsonAST2MongoValue._
  import JsonAST2JValue._
  def main(args: Array[String]){

    JObject(JField("foo", JString("bar")) :: Nil) \ "foo" match{
      case JString(value) => println(value)
      case _ => println("not found")
    }

    MongoArray(MongoObject(MongoField("baz", MongoString("bar")) :: Nil) :: Nil) \ "baz" match{
      case MongoString(value) => println(value)
      case _ => println("not found")
    }

//    testConact
  }

//  def testConact{
//    import JValueProvider._
//
////    println(JsonAST2.concat(JString("bar"), JObject(JField("foo", JString("baz")) :: Nil)))
//  }
}