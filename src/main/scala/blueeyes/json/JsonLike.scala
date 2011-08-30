package blueeyes.json

import scalaz.Zero

trait JsonLikeTypes[A, F <: A]{
  type or[L,R] = Either[L,R]
  type Obj = List[F]
  type Arr = List[A]
}

trait JsonLike[A, F <: A] extends JsonLikeTypes[A, F] with Zero[A]{

  def fold[Z](value: A, prim: A => Z, field: (String, A) => Z, arr: List[A] => Z, obj: List[F] => Z): Z

  def jsonField(name: String, value: A): F

  def jsonArray(elements: List[A]): A

  def jsonObject(elements: List[F]): A
}

class RichJson[A, F <: A](json: A, jsonLike: JsonLike[A, F])(implicit af: Manifest[A], mf: Manifest[F]) extends JsonLikeTypes[A, F]{
  /** XPath-like expression to query JSON fields by name. Matches only fields on
   * next level.
   * <p>
   * Example:<pre>
   * json \ "name"
   * </pre>
   */
  def +\ (nameToFind: String): A = {
    val p = (json: A) => {
      fieldName(json) match {
        case Some(name) if name == nameToFind => true
        case _ => false
      }
    }
    val found = findDirect(children(json), p)
    found match {
      case Nil => jsonLike.zero
      case x :: Nil => unfoldOne(x)
      case xs => jsonLike.jsonArray(xs.map(unfoldOne))
    }
  }

  private def unfold(json: A): Option[Obj or Arr] = jsonLike.fold(json, _ => None, (_, _) => None, v => Some(Right(v)), v => Some(Left(v)))
  private def fieldName(json: A): Option[String]  = jsonLike.fold(json, _ => None, (name, v) => Some(name), _ => None, _ => None)
  private def unfoldField(json: A): Option[A]     = jsonLike.fold(json, _ => None, (name, v) => Some(v), _ => None, _ => None)
  private def unfoldOne(json: A): A               = unfoldField(json) match{
    case Some(x) => x
    case None    => json
  }

  private def children(json: A): List[A] = unfold(json) match{
    case Some(Left(fields))    => fields
    case Some(Right(elements)) => elements
    case None => fieldName(json) match {
      case Some(x) => unfoldOne(json) :: Nil
      case None => Nil
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

    val unfolded = unfoldOne(json)
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
          unfold(head) match{
            case Some(Left(fields)) => nextQueue.enqueue(fields.map(unfoldOne(_)))
            case Some(Right(elements)) => nextQueue.enqueue(elements)
            case None => nextQueue
          }
        )
      }
    }

    breadthFirst0(Nil, Queue.empty.enqueue(json)).reverse
  }

  private def !!(json: A) = new RichJson[A, F](json, jsonLike)

  def set(path: JPath, value: A): A = if (path == JPath.Identity) value else {
    def up(l: List[A], i: Int, v: A) = l.length match {
        case len if len == i =>  l :+ v
        case len if i < 0 || i > len =>
          sys.error("Attempt to create a new element out of JArray bounds at " + i)

        case _ => l.updated(i, v)
    }

    unfold(json) match {
      case Some(Left(fields)) => path.nodes match {
        case JPathField(name) :: Nil => jsonLike.jsonObject(jsonLike.jsonField(name, value) :: fields.filterNot(fieldName(_).map(_ == name).getOrElse(false)))

        case JPathField(name)  :: nodes => jsonLike.jsonObject(jsonLike.jsonField(name, !!(+\(name)).set(JPath(nodes), value)) :: fields.filterNot(fieldName(_).map(_ == name).getOrElse(false)))

        case _ => sys.error("Objects are not indexed")
      }
      case Some(Right(elements)) => path.nodes match {
        case JPathIndex(index) :: Nil => jsonLike.jsonArray(up(elements, index, value))
        case JPathIndex(index) :: nodes => jsonLike.jsonArray(up(elements, index, !!(elements.lift(index).getOrElse(jsonLike.zero)).set(JPath(nodes), value)))
        case _ => sys.error("Arrays have no fields")
      }
      case _ => path.nodes match {
        case Nil => value

        case JPathIndex(_) :: _ => !!(jsonLike.jsonArray(Nil)).set(path, value)

        case JPathField(_) :: _ => !!(jsonLike.jsonObject(Nil)).set(path, value)
      }
    }
  }

  private def findDirect(xs: List[A], p: A => Boolean): List[A] = xs.flatMap { element =>
    unfold(element) match {
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
  implicit def toRichJson[A, F <: A](value: A)(implicit jsonLike: JsonLike[A, F], af: Manifest[A], mf: Manifest[F]) = new RichJson[A, F](value, jsonLike)
}