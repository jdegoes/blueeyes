package blueeyes.json

import blueeyes.json.JsonAST._

class JsonLikeElement[T]
case class Prim[A](value: A)                extends JsonLikeElement[A]
case class Arr[A](value: List[A])           extends JsonLikeElement[A]
case class Obj[A](value: List[A])           extends JsonLikeElement[A]

sealed trait ElementWitness[T]

trait JsonLike[A] {

  def unfold(value: A): JsonLikeElement[A]

  def unfoldOne(value: A): A

  def zero: A

  def fold[T: ElementWitness](value: T): A

  def children(value: A): List[A]

  def name(value: A): Option[String]
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
    val p = (json: A) => jsonLike.name(json) match {
      case Some(name) if name == nameToFind => true
      case _ => false
    }
    val found = findDirect(jsonLike.children(json), p)
    found match {
      case Nil => jsonLike.zero
      case x :: Nil => jsonLike.unfoldOne(x)
      case xs => jsonLike.fold(xs.map(jsonLike.unfoldOne))
    }
  }

  private def findDirect(xs: List[A], p: A => Boolean): List[A] = xs.flatMap {
    jsonLike.unfold(_) match {
      case Obj(l) => l.filter {
        case x if p(x) => true
        case _ => false
      }
      case Arr(l) => findDirect(l, p)
      case Prim(x) if p(x) => x :: Nil
      case _ => Nil
    }
  }
}

trait ToRichJsonLike{
  implicit def toRichJsonLike[A: JsonLike](value: A) = new RichJsonLike[A](value, implicitly[JsonLike[A]])
}

class JsonLikeJValue extends JsonLike[JValue]{

  def unfold(value: JValue) = value match{
    case JObject(fields)  => Obj(fields)
    case JArray(elements) => Arr(elements)
    case _                => Prim(value)
  }

  def unfoldOne(value: JValue) = value match{
    case JField(n, v) => v
    case _ => value
  }

  def children(value: JValue): List[JValue] = value match {
    case JObject(l)   => l
    case JArray(l)    => l
    case JField(n, v) => List(v)
    case _ => Nil
  }

  def fold[T: ElementWitness](value: T) = value match {
    case e: List[JValue]           => JArray(e)
    case e: List[(String, JValue)] => JObject(e.map(v => JField(v._1, v._2)))
    case e: JValue                 => e
  }

  val zero: JValue = JNothing

  def name(value: JValue) = value match{
    case JField(n, v) => Some(n)
    case _ => None
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