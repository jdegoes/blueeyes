package blueeyes.json

import blueeyes.json.JsonAST._

class JsonLikeElement[T]
case class Prim[A](value: A)                extends JsonLikeElement[A]
case class Elem[A](name: String, value: A)  extends JsonLikeElement[A]
case class Arr[A](value: List[A])           extends JsonLikeElement[A]
case class Obj[A](value: List[(String, A)]) extends JsonLikeElement[A]

sealed trait ElementWitness[T]

trait JsonLike[A] {

  protected implicit def toPrim(value: A)                = Prim[A](value)
  protected implicit def toArr(value: List[A])           = Arr[A](value)
  protected implicit def toObj(value: List[(String, A)]) = Obj[A](value)
  protected implicit def toElem(value: (String, A))      = Elem[A](value._1, value._2)

  def unfold(value: A): JsonLikeElement[A]

  def unfoldOne(value: A): (Option[String], A)

  def zero: A

  def fold[T: ElementWitness](value: T): A

  def foldOne(name: String, value: A): A

  def children(value: A): List[A]
}

class RichJsonLike[A](json: A, jsonLike: JsonLike[A]){
  implicit case object PrimWitness extends ElementWitness[A]
  implicit case object ArrWitness  extends ElementWitness[List[A]]
  implicit case object ObjWitness  extends ElementWitness[List[(String, A)]]

   /** XPath-like expression to query JSON fields by name. Matches only fields on
   * next level.
   * <p>
   * Example:<pre>
   * json \ "name"
   * </pre>
   */
  def +\ (nameToFind: String): A = {
    val p = (name: Option[String], json: A) => (name, json) match {
      case (Some(n), value) if n == nameToFind => true
      case _ => false
    }
    findDirect(jsonLike.children(json), p) match {
      case Nil => jsonLike.zero
      case x :: Nil => jsonLike.unfoldOne(x)._2
      case xs => jsonLike.fold(xs.map(jsonLike.unfoldOne(_)).map(_._2))
    }
  }

  private def findDirect(xs: List[A], p: (Option[String], A) => Boolean): List[A] = xs.flatMap {
    jsonLike.unfold(_) match{
      case Obj(l) => l.filter {
        case x if p(Some(x._1), x._2) => true
        case _ => false
      }.map(x => jsonLike.foldOne(x._1, x._2))
      case Arr(l) => findDirect(l, p)
      case Elem(n, x) if p(Some(n), x) => jsonLike.foldOne(n, x) :: Nil
      case Prim(x) if p(None, x) => x :: Nil
      case _ => Nil
    }
  }
}

trait ToRichJsonLike{
  implicit def toRichJsonLike[A: JsonLike](value: A) = new RichJsonLike[A](value, implicitly[JsonLike[A]])
}

class JsonLikeJValue extends JsonLike[JValue]{
  def unfold(value: JValue) = value match{
    case JObject(fields)  => fields.map(field => (field.name, field.value))
    case JArray(elements) => elements
    case JField(n, v)     => (n, v)
    case _                => value
  }

  def unfoldOne(value: JValue) = value match{
    case JField(n, v) => (Some(n), v)
    case _ => (None, value)
  }

  def fold[T: ElementWitness](value: T) = value match {
    case e: List[JValue]           => JArray(e)
    case e: List[(String, JValue)] => JObject(e.map(v => JField(v._1, v._2)))
    case e: JValue                 => e
  }

  def foldOne(name: String, value: JValue) = JField(name, value)

  val zero: JValue = JNothing

  def children(value: JValue): List[JValue] = value match {
    case JObject(l) => l
    case JArray(l) => l
    case JField(n, v) => List(v)
    case _ => Nil
  }
}

trait JsonLikeJValueImplicits{
  implicit val jsonLikeJValue = new JsonLikeJValue()
}
object JsonLikeJValueImplicits extends JsonLikeJValueImplicits

object JsonLikeTest extends JsonLikeJValueImplicits with ToRichJsonLike{
  def main(args: Array[String]){
    val value: JValue = JObject(JField("bar", JString("dd")) :: Nil)

    println(value.\("bar"))
    println(value.+\("bar"))
  }
}