package blueeyes.json

import blueeyes.json.JsonAST._

trait JsonLikeTypes[A]{
  type or[L,R] = Either[L,R]
  type Obj = List[A]
  type Arr = List[A]
}

trait JsonLike[A, F <: A] extends JsonLikeTypes[A] {

  def unfold(value: A): Option[Obj or Arr]

  def unfoldOne(value: A): A

  def zero: A

  def foldArr(elemets: List[A]): A

  def foldObj(elemets: List[F]): A

  def children(value: A): List[A]

  def name(value: A): Option[String]
}

class RichJsonLike[A, F <: A](json: A, jsonLike: JsonLike[A, F])(implicit af: Manifest[A], mf: Manifest[F]) extends JsonLikeTypes[A]{
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
      case xs => jsonLike.foldArr(xs.map(jsonLike.unfoldOne))
    }
  }

  /**
   * Returns the element as a JValue of the specified class.
   * <p>
   * Example:<pre>
   * (json \ "foo" --> classOf[JField]).value
   * </pre>
   */
  def --> [B <: A](clazz: Class[B]): B = (this -->? clazz).getOrElse(sys.error("Expected class " + clazz + ", but found: " + mf.erasure.asInstanceOf[Class[A]]))
  /**
  * Returns the element as an option of a JValue of the specified class.
  * <p>
  * Example:<pre>
  * (json \ "foo" -->? classOf[JField]).map(_.value).getOrElse(defaultFieldValue)
  * </pre>
  */
  def -->? [B <: A](clazz: Class[B]): Option[B] = {
    val ca: Class[A] = mf.erasure.asInstanceOf[Class[A]]
    val cf: Class[F] = mf.erasure.asInstanceOf[Class[F]]
    def extractTyped(value: A) = if (ca == clazz) Some(value.asInstanceOf[B]) else None

    val unfolded = jsonLike.unfoldOne(json)
    if (unfolded != json && clazz != cf) extractTyped(unfolded)
    else extractTyped(json)
  }

  /**
   * Does a breadth-first traversal of all descendant JValues, beginning
   * with this one.
   */
  def breadthFirst: List[A] = {
    import scala.collection.immutable.Queue

    def breadthFirst0(cur: List[A], queue: Queue[A]): List[A] = {
      if (queue.isEmpty) cur
      else {
        val (head, nextQueue) = queue.dequeue

        breadthFirst0(head :: cur,
          jsonLike.unfold(head) match{
            case Some(Left(fields)) => nextQueue.enqueue(fields.map(jsonLike.unfoldOne(_)))
            case Some(Right(elements)) => nextQueue.enqueue(elements)
            case None => nextQueue
          }
        )
      }
    }

    breadthFirst0(Nil, Queue.empty.enqueue(json)).reverse
  }

  private def findDirect(xs: List[A], p: A => Boolean): List[A] = xs.flatMap { element =>
    jsonLike.unfold(element) match {
      case Some(Left(l)) => l.filter {
        case x if p(x) => true
        case _ => false
      }
      case Some(Right(l)) => findDirect(l, p)
      case None if p(element) => element  :: Nil
      case _ => Nil
    }
  }
}

trait ToRichJsonLike{
  implicit def toRichJsonLike[A, F <: A](value: A)(implicit jsonLike: JsonLike[A, F], af: Manifest[A], mf: Manifest[F]) = new RichJsonLike[A, F](value, jsonLike)
}