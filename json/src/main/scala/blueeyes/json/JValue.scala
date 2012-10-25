/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package blueeyes.json

import scalaz._
import scalaz.Ordering._
import scalaz.Validation._
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.math.bigInt._
import scalaz.std.string._
import scalaz.std.tuple._

import scalaz.syntax.applicative._
import scalaz.syntax.bifunctor._
import scalaz.syntax.order._
import scalaz.syntax.semigroup._

import scala.annotation.{switch, tailrec}
import scala.collection.mutable

import java.lang.Character.codePointAt
import JValue.{RenderMode, Compact, Pretty, Canonical}

/**
 * Data type for Json AST.
 */
sealed trait JValue extends Merge.Mergeable with Diff.Diffable with Product with Ordered[JValue] { self =>
  def toOption = if (self == JUndefined) None else Some(self)

  def getOrElse(that: => JValue) = if (self == JUndefined) that else self

  def compare(that: JValue): Int = JValue.order.order(self, that).toInt

  def sort: JValue

  def renderCompact: String

  def renderPretty: String = renderCompact

  def renderCanonical: String = renderCompact

  protected[json] def internalRender(sb: StringBuilder, mode: RenderMode, indent: String) {
    sb.append(renderCompact)
  }

  /** XPath-like expression to query JSON fields by name. Matches only fields on
   * next level.
   */
  def \ (nameToFind: String): JValue = self match {
    case j @ JObject(fields) => j.get(nameToFind)
    case j @ JArray(elements) => 
      elements.map(_ \ nameToFind) match {
        case Nil => JUndefined
        case x :: Nil => x
        case elements @ x :: xs => JArray(elements)
      }
    case _ => JUndefined
  }

  def \? (nameToFind: String): Option[JValue] = (self \ nameToFind).toOption

  /**
   * Returns the element as a JValue of the specified class.
   */
  def --> [A <: JValue](clazz: Class[A]): A = (self -->? clazz).getOrElse(sys.error("Expected class " + clazz + ", but found: " + self.getClass))

  /**
   * Returns the element as an option of a JValue of the specified class.
   */
  def -->? [A <: JValue](clazz: Class[A]): Option[A] = if (self.getClass == clazz) Some(self.asInstanceOf[A]) else None

  /** 
   * Does a breadth-first traversal of all descendant JValues, beginning
   * with this one.
   */
  def breadthFirst: List[JValue] = {
    import scala.collection.immutable.Queue

    def breadthFirst0(cur: List[JValue], queue: Queue[JValue]): List[JValue] = {
      if (queue.isEmpty) cur
      else {
        val (head, nextQueue) = queue.dequeue

        breadthFirst0(head :: cur, 
          head match {
            case JObject(fields) =>
              nextQueue.enqueue(fields.values.toList)

            case JArray(elements) =>
              nextQueue.enqueue(elements)

            case jvalue => nextQueue
          }
        )
      }
    }

    breadthFirst0(Nil, Queue.empty.enqueue(self)).reverse
  }

  /** XPath-like expression to query JSON fields by name. Returns all matching fields.
   */
  def \\(nameToFind: String): JValue = {
    def find(json: JValue): List[JValue] = json match {
      case JObject(l) => l.foldLeft(List.empty[JValue]) {
        case (acc, field) => 
          val newAcc = if (field._1 == nameToFind) field._2 :: acc else acc

          newAcc ::: find(field._2)
      }

      case JArray(l) => l.flatMap(find)
      
      case _ => Nil
    }
    find(self) match {
      case x :: Nil => x
      case x => JArray(x)
    }
  }

  /** Gets the specified value located at the terminal of the specified path.
   */
  def apply(path: JPath): JValue = get(path)

  def get(path: JPath): JValue = path.extract(self)

  def insert(path: JPath, value: JValue): Validation[Throwable, JValue] = {
    value match {
      case JUndefined => success[Throwable, JValue](this)
      case value => Validation fromTryCatch { unsafeInsert(path, value) }
    }
  }

  /**
   * A safe merge function that ensures that values are not overwritten.
   */
  def insertAll(other: JValue): ValidationNEL[Throwable, JValue] = {
    other.flattenWithPath.foldLeft[ValidationNEL[Throwable, JValue]](success(self)) {
      case (acc, (path, value)) => acc flatMap { (_: JValue).insert(path, value).toValidationNEL }
    }
  }

  def unsafeInsert(rootPath: JPath, rootValue: JValue): JValue = {
    JValue.unsafeInsert(self, rootPath, rootValue)
  }

  def set(path: JPath, value: JValue): JValue = if (path == JPath.Identity) value else {
    def arraySet(l: List[JValue], i: Int, rem: JPath, v: JValue): List[JValue] = {
      def update(l: List[JValue], j: Int): List[JValue] = l match {
        case x :: xs => (if (j == i) x.set(rem, v) else x) :: update(xs, j + 1)
        case Nil => Nil
      }

      update(l.padTo(i + 1, JUndefined), 0)
    }

    self match {
      case obj @ JObject(fields) => path.nodes match {
        case JPathField(name) :: nodes => 
          val (child, rest) = obj.partitionField(name)

          rest + JField(name, child.set(JPath(nodes), value))

        case x => sys.error("Objects are not indexed: attempted to set " + path + " on " + self)
      }

      case arr @ JArray(elements) => path.nodes match {
        case JPathIndex(index) :: nodes => JArray(arraySet(elements, index, JPath(nodes), value))
        case x => sys.error("Arrays have no fields: attempted to set " + path + " on " + self)
      }

      case _ => path.nodes match {
        case Nil => value
        case JPathIndex(_) :: _ => JArray(Nil).set(path, value)
        case JPathField(_) :: _ => JObject(Nil).set(path, value)
      }
    }
  }

  def delete(path: JPath): Option[JValue] = {
    path.nodes match {
      case JPathField(name) :: xs => this match {
        case JObject(fields) => Some(
          JObject(fields flatMap {
            case JField(`name`, value) => value.delete(JPath(xs: _*)) map { v => JField(name, v) }
            case unmodified => Some(unmodified)
          })
        )
        case unmodified => Some(unmodified)
      }

      case JPathIndex(idx) :: xs => this match {
        case JArray(elements) => Some(JArray(elements.zipWithIndex.flatMap { case (v, i) => if (i == idx) v.delete(JPath(xs: _*)) else Some(v) }))
        case unmodified => Some(unmodified)
      }

      case Nil => None
    }
  }

  /** Return nth element from JSON.
   * Meaningful only to JArray, JObject and JField. Returns JUndefined for other types.
   */
  def apply(i: Int): JValue = JUndefined

  /** Return direct child elements.
   */
  def children: Iterable[JValue] = self match {
    case JObject(fields) => fields.values
    case JArray(l) => l
    case _ => List.empty
  }

  /** Return a combined value by folding over JSON by applying a function <code>f</code>
   * for each element. The initial value is <code>z</code>.
   */
  def foldDown[A](z: A)(f: (A, JValue) => A): A = foldDownWithPath(z) { (acc, p, v) => f(acc, v) }

  /** Return a combined value by folding over JSON by applying a function `f`
   * for each element, passing along the path to the elements. The initial
   * value is `z`.
   */
  def foldDownWithPath[A](z: A)(f: (A, JPath, JValue) => A): A = {
    def rec(acc: A, p: JPath, v: JValue): A = {
      val newAcc = f(acc, p, v)

      v match {
        case JObject(l)   =>
          l.foldLeft(newAcc) { 
            case(acc, field) =>
              rec(acc, p \ field._1, field._2) 
          }

        case JArray(l) =>
          l.zipWithIndex.foldLeft(newAcc) { case (a, (e, idx)) => rec(a, p \ idx, e) }

        case _ => newAcc
      }
    }

    rec(z, JPath.Identity, self)
  }

  /** Return a combined value by folding over JSON by applying a function <code>f</code>
   * for each element. The initial value is <code>z</code>.
   */
  def foldUp[A](z: A)(f: (A, JValue) => A): A = foldUpWithPath(z) { (acc, p, v) => f(acc, v) }

  /** Return a combined value by folding over JSON by applying a function `f`
   * for each element, passing along the path to the elements. The initial
   * value is `z`.
   */
  def foldUpWithPath[A](z: A)(f: (A, JPath, JValue) => A): A = {
    def rec(acc: A, p: JPath, v: JValue): A = {
      f(v match {
        case JObject(l) =>
          l.foldLeft(acc) { 
            (acc, field) => 
              rec(acc, p \ field._1, field._2)
          }

        case JArray(l) =>
          l.zipWithIndex.foldLeft(acc) { (a, t) => val (e, idx) = t; rec(a, p \ idx, e) }

        case _ => acc
      }, p, v)
    }

    rec(z, JPath.Identity, self)
  }

  /** Return a new JValue resulting from applying the given function <code>f</code>
   * to each element, moving from the bottom-up.
   */
  def mapUp(f: JValue => JValue): JValue = mapUpWithPath((p, j) => f(j))

  /** Return a new JValue resulting from applying the given function <code>f</code>
   * to each element and its path, moving from the bottom-up.
   */
  def mapUpWithPath(f: (JPath, JValue) => JValue): JValue = {
    def rec(p: JPath, v: JValue): JValue = v match {
      case JObject(l) => 
        f(p, JObject(l.flatMap { f => 
          val v2 = rec(p \ f._1, f._2)

          if (v2 == JUndefined) Nil else JField(f._1, v2) :: Nil
          
        }))

      case JArray(l) => f(p, JArray(l.zipWithIndex.flatMap(t => rec(p \ t._2, t._1) match {
        case JUndefined => Nil
        case x => x :: Nil
      })))

      case x => f(p, x)
    }
    rec(JPath.Identity, self)
  }

  /** Return a new JValue resulting from applying the given function <code>f</code>
   * to each element, moving from the top-down.
   */
  def mapDown(f: JValue => JValue): JValue = mapDownWithPath((p, j) => f(j))

  /** Return a new JValue resulting from applying the given function <code>f</code>
   * to each element and its path, moving from the top-down.
   */
  def mapDownWithPath(f: (JPath, JValue) => JValue): JValue = {
    def rec(p: JPath, v: JValue): JValue = {
      f(p, v) match {
        case JObject(l) =>
          JObject(l.flatMap { f =>
            val v2 = rec(p \ f._1, f._2)

            if (v2 == JUndefined) Nil else JField(f._1, v2) :: Nil
          })

        case JArray(l) =>
          JArray(l.zipWithIndex.flatMap { t =>
            val (e, idx) = t

            rec(p \ idx, e) match {
              case JUndefined => Nil
              case x => x :: Nil
            }
          })

        case x => x
      }
    }

    rec(JPath.Identity, self)
  }

  /** Return a new JValue resulting from applying the given partial function <code>f</code>
   * to each element in JSON.
   */
  def transform(f: PartialFunction[JValue, JValue]): JValue = mapUp { x =>
    if (f.isDefinedAt(x)) f(x) else x
  }

  /** Replaces the matched path values with the result of calling the
   * replacer function on the matches. If the path has no values, the
   * method has no effect -- i.e. it is not an error to specify paths
   * which do not exist.
   */
  def replace(target: JPath, replacer: JValue => JValue): JValue = {
    def replace0(target: JPath, j: JValue): JValue = target.nodes match {
      case Nil => replacer(j)

      case head :: tail => head match {
        case JPathField(name1) => j match {
          case JObject(fields) => JObject(fields.map {
            case JField(name2, value) if (name1 == name2) => JField(name1, replace0(JPath(tail: _*), value))

            case field => field
          })

          case jvalue => jvalue
        }

        case JPathIndex(index) => j match {
          case JArray(elements) =>
            val split = elements.splitAt(index)

            val prefix = split._1
            val middle = replace0(JPath(tail: _*), split._2.head)
            val suffix = split._2.drop(1)

            JArray(prefix ++ (middle :: suffix))

          case jvalue => jvalue
        }
      }
    }

    target.expand(self).foldLeft(self) { (jvalue, expansion) =>
      replace0(expansion, jvalue)
    }
  }

  /** A shorthand for the other replacement in the case that the replacement
   * does not depend on the value being replaced.
   */
  def replace(target: JPath, replacement: JValue): JValue = replace(target, r => replacement)

  /** Return the first element from JSON which matches the given predicate.
   */
  def find(p: JValue => Boolean): Option[JValue] = {
    def find(json: JValue): Option[JValue] = {
      if (p(json)) return Some(json)
      json match {
        case JObject(l) => l.map(_._2).flatMap(find).headOption
        case JArray(l) => l.flatMap(find).headOption
        case _ => None
      }
    }
    find(self)
  }

  /** Return a List of all elements which matches the given predicate.
   */
  def filter(p: JValue => Boolean): List[JValue] =
    foldDown(List.empty[JValue])((acc, e) => if (p(e)) e :: acc else acc).reverse

  def flatten: List[JValue] =
    foldDown(List.empty[JValue])((acc, e) => e :: acc).reverse

  /** Flattens the JValue down to a list of path to simple JValue primitive.
   */
  def flattenWithPath: Vector[(JPath, JValue)] = {
    def flatten0(path: JPath)(value: JValue): Vector[(JPath, JValue)] = value match {
      case JObject.empty => Vector((path -> value))

      case JObject(fields) => 
        fields.foldLeft(Vector.empty[(JPath, JValue)]) { 
          case (acc, field) =>
            acc ++ flatten0(path \ field._1)(field._2)
        }
      
      case JArray(Nil) => Vector((path -> value))

      case JArray(elements) => 
        Vector(elements: _*).zipWithIndex.flatMap { tuple =>
          val (element, index) = tuple

          flatten0(path \ index)(element)
        }

      case _ => Vector((path -> value))
    }

    flatten0(JPath.Identity)(self)
  }

  /** Concatenate with another JSON.
   * This is a concatenation monoid: (JValue, ++, JUndefined)
   */
  def ++ (other: JValue): JValue = {
    def append(value1: JValue, value2: JValue): JValue = (value1, value2) match {
      case (JUndefined, x) => x
      case (x, JUndefined) => x
      case (JArray(xs), JArray(ys)) => JArray(xs ::: ys)
      case (JArray(xs), v: JValue) => JArray(xs ::: List(v))
      case (v: JValue, JArray(xs)) => JArray(v :: xs)
      case (x, y) => JArray(x :: y :: Nil)
    }
    append(self, other)
  }

  /** Return a JSON where all elements matching the given predicate are removed.
   */
  def remove(p: JValue => Boolean): JValue = self mapUp {
    case x if p(x) => JUndefined
    case x => x
  }

  /**
   * Remove instances of Nothing from the data structure.
   */
  def minimize: Option[JValue] = {
    this match {
      case JObject(fields)  => Some(JObject(fields flatMap { case JField(k, v) => v.minimize.map(JField(k, _)) }))
      case JArray(elements) => Some(JArray(elements.flatMap(_.minimize)))
      case JUndefined => None
      case value => Some(value)
    }
  }

  override def toString = renderCompact
}

object JValue {
  sealed trait RenderMode
  case object Pretty extends RenderMode
  case object Compact extends RenderMode
  case object Canonical extends RenderMode

  def apply(p: JPath, v: JValue) = JUndefined.set(p, v)

  private def unflattenArray(elements: Seq[(JPath, JValue)]): JArray = {
    elements.foldLeft(JArray(Nil)) { (arr, t) => arr.set(t._1, t._2) --> classOf[JArray] }
  }

  private def unflattenObject(elements: Seq[(JPath, JValue)]): JObject = {
    elements.foldLeft(JObject(Nil)) { (obj, t) => obj.set(t._1, t._2) --> classOf[JObject] }
  }

  def unflatten(elements: Seq[(JPath, JValue)]): JValue = {
    if (elements.isEmpty) JUndefined
    else {
      val sorted = elements.sortBy(_._1)

      val (xp, xv) = sorted.head

      if (xp == JPath.Identity && sorted.size == 1) xv
      else if (xp.path.startsWith("[")) unflattenArray(sorted)
      else unflattenObject(sorted)
    }      
  }    

  case class paired(jv1: JValue, jv2: JValue) {
    assert(jv1 != null && jv2 != null)
    final def fold[A](default: => A)(
      obj:    Map[String, JValue] => Map[String, JValue] => A,
      arr:    List[JValue] => List[JValue] => A,
      str:    String => String => A,
      num:    BigDecimal => BigDecimal => A,
      bool:   Boolean => Boolean => A,
      nul:    => A, nothing: => A) = {
      (jv1, jv2) match {
        case (JObject(o1),  JObject(o2)) => obj(o1)(o2)
        case (JArray(a1) ,  JArray(a2))  => arr(a1)(a2)
        case (JString(s1),  JString(s2)) => str(s1)(s2)
        case (JBool(b1),    JBool(b2)) => bool(b1)(b2)
        case (JNum(d1),     JNum(d2)) => num(d1)(d2)
        case (JNull,        JNull) => nul
        case (JUndefined,     JUndefined) => nul
        case _ => default
      }
    }
  }

  @inline final def typeIndex(jv: JValue) = jv match {
    case JObject(_)  => 7
    case JArray(_)   => 6
    case JString(_)  => 5
    case JNum(_)     => 4
    case JBool(_)    => 1
    case JNull       => 0
    case JUndefined    => -1
  }

  implicit final val jnumOrder: Order[JNum] = new Order[JNum] {
    def order(v1: JNum, v2: JNum) = Ordering.fromInt(v1 numCompare v2)
  }

  implicit final val order: Order[JValue] = new Order[JValue] {
    def order(jv1: JValue, jv2: JValue) = {
      (typeIndex(jv1) ?|? typeIndex(jv2)) |+|
      //since we can only get equality from values of the same type, we can just cast
      (jv1 match {
        case v: JObject => JObject.order(v, jv2.asInstanceOf[JObject])
        case v: JArray  => JArray.order(v, jv2.asInstanceOf[JArray])
        case v: JString => v.value ?|? jv2.asInstanceOf[JString].value
        case v: JNum    => v ?|? jv2.asInstanceOf[JNum]
        case v: JBool   => v.value ?|? jv2.asInstanceOf[JBool].value
        case _ => EQ
      })
    }
  } 

  def unsafeInsert(rootTarget: JValue, rootPath: JPath, rootValue: JValue): JValue = {
    def rec(target: JValue, path: JPath, value: JValue): JValue = {
      if ((target == JNull || target == JUndefined) && path == JPath.Identity) value else {
        def arrayInsert(l: List[JValue], i: Int, rem: JPath, v: JValue): List[JValue] = {
          def update(l: List[JValue], j: Int): List[JValue] = l match {
            case x :: xs => (if (j == i) rec(x, rem, v) else x) :: update(xs, j + 1)
            case Nil => Nil
          }

          update(l.padTo(i + 1, JUndefined), 0)
        }

        target match {
          case obj @ JObject(fields) => path.nodes match {
            case JPathField(name) :: nodes => 
              val (child, rest) = obj.partitionField(name)

              rest + JField(name, rec(child, JPath(nodes), value))

            case JPathIndex(_) :: _ => sys.error("Objects are not indexed: attempted to insert " + value + " at " + rootPath + " on " + rootTarget)
            case Nil => sys.error("JValue insert would overwrite existing data: " + target + " cannot be rewritten to " + value + " at " + path + 
                                " in unsafeInsert of " + rootValue + " at " + rootPath + " in " + rootTarget)
          }

          case arr @ JArray(elements) => path.nodes match {
            case JPathIndex(index) :: nodes => JArray(arrayInsert(elements, index, JPath(nodes), value))
            case JPathField(_) :: _ => sys.error("Arrays have no fields: attempted to insert " + value + " at " + rootPath + " on " + rootTarget)
            case Nil => sys.error("JValue insert would overwrite existing data: " + target + " cannot be rewritten to " + value + " at " + path + 
                                  " in unsafeInsert of " + rootValue + " at " + rootPath + " in " + rootTarget)
          }

          case JNull | JUndefined => path.nodes match {
            case Nil => value
            case JPathIndex(_) :: _ => rec(JArray(Nil), path, value)
            case JPathField(_) :: _ => rec(JObject(Nil), path, value)
          }

          case x => sys.error("JValue insert would overwrite existing data: " + x + " cannot be updated to " + value + " at " + path + 
                              " in unsafeInsert of " + rootValue + " at " + rootPath + " in " + rootTarget)
        }
      }
    }

    rec(rootTarget, rootPath, rootValue)
  }
}


case object JUndefined extends JValue {
  final def sort: JValue = this
  final def renderCompact: String = "null"
}

case object JNull extends JValue {
  final def sort: JValue = this
  final def renderCompact: String = "null"
}

sealed trait JBool extends JValue {
  def value: Boolean
  final def sort: JBool = this
}

case object JTrue extends JBool {
  final def value = true
  final def renderCompact: String = "true"
}

case object JFalse extends JBool {
  final def value = false
  final def renderCompact: String = "false"
}

object JBool {
  def apply(value: Boolean): JBool = if (value) JTrue else JFalse
  def unapply(value: JBool): Option[Boolean] = Some(value.value)
}

sealed trait JNum extends JValue {
  def toBigDecimal: BigDecimal
  def toLong: Long
  def toDouble: Double
  def toRawString: String
  final def renderCompact = toRawString
  def sort: JNum = this

  // SI-6173: to avoid hashCode on BigDecimal, we use a hardcoded hashcode to
  // ensure JNum("123.45").hashCode == JNum(123.45).hashCode
  override val hashCode = 6173

  protected[json] def numCompare(other: JNum): Int
}

case class JNumStr private[json] (value: String) extends JNum {
  lazy val dec: BigDecimal = BigDecimal(value)

  final def toBigDecimal: BigDecimal = dec
  final def toLong: Long = value.toDouble.toLong
  final def toDouble: Double = value.toDouble
  final def toRawString: String = value

  override def equals(other: Any) = other match {
    case JNumLong(n) => value == n.toString
    case num: JNum => toBigDecimal == num.toBigDecimal
    case _ => false
  }

  protected[json] def numCompare(other: JNum) = toBigDecimal compare other.toBigDecimal
}

case class JNumLong(value: Long) extends JNum {
  final def toBigDecimal: BigDecimal = BigDecimal(value)
  final def toLong: Long = value
  final def toDouble: Double = value.toDouble
  final def toRawString: String = value.toString

  override def equals(other: Any) = other match {
    case JNumLong(n) => value == n
    case JNumDouble(n) => value == n
    case JNumBigDec(n) => value == n
    case JNumStr(s) => value.toString == s
    case _ => false
  }

  protected[json] def numCompare(other: JNum) = other match {
    case JNumLong(n) => value compare n
    case JNumDouble(n) => value.toDouble compare n
    case JNumBigDec(n) => BigDecimal(value) compare n
    case _ => toBigDecimal compare other.toBigDecimal
  }
}

case class JNumDouble private[json] (value: Double) extends JNum {
  final def toBigDecimal: BigDecimal = BigDecimal(value)
  final def toLong: Long = value.toLong
  final def toDouble: Double = value
  final def toRawString: String = value.toString

  override def equals(other: Any) = other match {
    case JNumLong(n) => value == n
    case JNumDouble(n) => value == n
    case num: JNum => toBigDecimal == num.toBigDecimal
    case _ => false
  }

  protected[json] def numCompare(other: JNum) = other match {
    case JNumLong(n) => value compare n.toDouble
    case JNumDouble(n) => value compare n
    case JNumBigDec(n) => BigDecimal(value) compare n
    case _ => toBigDecimal compare other.toBigDecimal
  }
}

case class JNumBigDec(value: BigDecimal) extends JNum {
  final def toBigDecimal: BigDecimal = value
  final def toLong: Long = value.toLong
  final def toDouble: Double = value.toDouble
  final def toRawString: String = value.toString

  override def equals(other: Any) = other match {
    case JNumLong(n) => value == n
    case JNumDouble(n) => value == n
    case JNumBigDec(n) => value == n
    case num: JNum => value == num.toBigDecimal
    case _ => false
  }

  protected[json] def numCompare(other: JNum) = other match {
    case JNumLong(n) => value compare n
    case JNumDouble(n) => value compare n
    case JNumBigDec(n) => value compare n
    case _ => value compare other.toBigDecimal
  }
}

case object JNum {
  def apply(value: Double): JValue = if (value.isNaN || value == Double.PositiveInfinity || value == Double.NegativeInfinity) JUndefined else JNumDouble(value)

  private[json] def apply(value: String): JNum = JNumStr(value)

  def apply(value: Long): JNum = JNumLong(value)

  def apply(value: BigDecimal): JNum = JNumBigDec(value)
  
  def compare(v1: JNum, v2: JNum) = v1.numCompare(v2)

  def unapply(value: JNum): Option[BigDecimal] = {
    try { Some(value.toBigDecimal) }
    catch { case _ : NumberFormatException => None }
  }
}

case class JString(value: String) extends JValue {
  final def sort: JString = this
  final def renderCompact: String = JString.escape(value)
  final override def renderPretty: String = JString.escape(value)
  final override def internalRender(sb: StringBuilder, mode: RenderMode, indent: String) {
    JString.internalEscape(sb, value)
  }
}

object JString {
  final def escape(s: String): String = buildString(internalEscape(_, s))

  protected[json] final def internalEscape(sb: StringBuilder, s: String) {
    sb.append('"')
    var i = 0
    val len = s.length
    while (i < len) {
      (s.charAt(i): @switch) match {
        case '"' => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case c =>
          if (c < ' ')
            sb.append("\\u%04x" format c)
          else
            sb.append(c)
      }
      i += 1
    }
    sb.append('"')
  }
}

case object JField extends ((String, JValue) => JField) {
  def apply(name: String, value: JValue): JField = (name, value)

  def unapply(value: JField): Option[(String, JValue)] = Some(value)

  implicit final val order: Order[JField] = new Order[JField] {
    def order(f1: JField, f2: JField) = (f1._1 ?|? f2._1) |+| (f1._2 ?|? f2._2)
  }

  implicit final val ordering = order.toScalaOrdering

  def liftFilter(f: JField => Boolean): JValue => JValue = (value: JValue) => value match {
    case JObject(fields) => JObject(fields.filter(f))
    case _ => value
  }

  def liftFind(f: JField => Boolean): JValue => Boolean = (jvalue: JValue) => {
    jvalue match {
      case JObject(fields) => !fields.filter(f).isEmpty
      case _ => false
    }
  }

  def liftMap(f: JField => JField): JValue => JValue = (jvalue: JValue) => {
    jvalue match {
      case JObject(fields) => JObject(fields.map(f))
      case _ => jvalue
    }
  }

  def liftCollect(f: PartialFunction[JField, JField]): PartialFunction[JValue, JValue] = new PartialFunction[JValue, JValue] {
    def isDefinedAt(value: JValue): Boolean = !applyOpt(value).isEmpty

    def apply(value: JValue): JValue = applyOpt(value).get

    def applyOpt(value: JValue): Option[JValue] = value match {
      case JObject(fields) => 
        val newFields = fields.collect(f)

        if (newFields.isEmpty) None else Some(JObject(newFields))

      case _ => None
    }
  }
}

case class JObject(fields: Map[String, JValue]) extends JValue {
  def get(name: String): JValue = fields.get(name).getOrElse(JUndefined)

  def + (field: JField): JObject = copy(fields = fields + field)

  def - (name: String): JObject = copy(fields = fields - name)

  def partitionField(field: String): (JValue, JObject) = {
    (get(field), JObject(fields - field))
  }
  
  def partition(f: JField => Boolean): (JObject, JObject) = {
    fields.partition(f).bimap(JObject(_), JObject(_))
  }

  def sort: JObject = JObject(fields.filter(_._2 ne JUndefined).map(_.map(_.sort)))

  def mapFields(f: JField => JField) = JObject(fields.map(f))

  def isNested: Boolean = fields.values.exists {
    case _: JArray => true
    case _: JObject => true
    case _ => false
  }

  final override def renderCompact: String = buildString(internalRender(_, Compact, ""))

  final override def renderPretty: String = buildString(internalRender(_, Pretty, ""))

  final override def renderCanonical: String = buildString(internalRender(_, Canonical, ""))

  final override def internalRender(sb: StringBuilder, mode: RenderMode, indent: String) {
    mode match {
      case Compact =>
        // render as compactly as possible
        sb.append("{")
        var seen = false
        fields.foreach {
          case (k, v) =>
            if (seen) sb.append(",") else seen = true
            JString.internalEscape(sb, k)
            sb.append(":")
            v.internalRender(sb, Compact, "")
        }
        sb.append("}")

      case _ => 
        val indent2 = indent + "  "
        val oneLine = fields.size < 4 && !isNested
        val delimiter = if (oneLine) ", " else ",\n"

        sb.append("{")
        if (!oneLine) sb.append("\n")

        var seen = false
        def handlePair(tpl: (String, JValue)) {
          if (seen) sb.append(delimiter) else seen = true
          if (!oneLine) sb.append(indent2)
          JString.internalEscape(sb, tpl._1)
          sb.append(": ")
          tpl._2.internalRender(sb, mode, indent2)
        }

        if (mode == Canonical)
          fields.toSeq.sorted.foreach(handlePair)
        else
          fields.foreach(handlePair)

        if (!oneLine) {
          sb.append("\n")
          sb.append(indent)
        }
        sb.append("}")
    }
  }
}

case object JObject extends (Map[String, JValue] => JObject) {
  final val empty = JObject(Nil)
  final implicit val order: Order[JObject] = Order[List[JField]].contramap((_: JObject).fields.toList.sorted.filter(_._2 ne JUndefined).sortBy(_._2))

  def apply(fields: Traversable[JField]): JObject = JObject(fields.toMap)

  def apply(fields: JField*): JObject = JObject(fields.toMap)

  def unapplySeq(value: JValue): Option[Seq[JField]] = value match {
    case JObject(fields) => Some(fields.toSeq)

    case _ => None
  }
}

case class JArray(elements: List[JValue]) extends JValue {
  def sort: JArray = JArray(elements.filter(_ ne JUndefined).map(_.sort).sorted)

  override def apply(i: Int): JValue = elements.lift(i).getOrElse(JUndefined)

  def isNested: Boolean = elements.exists {
    case _: JArray => true
    case _: JObject => true
    case _ => false
  }

  final override def renderCompact: String = buildString(internalRender(_, Compact, ""))

  final override def renderPretty: String = buildString(internalRender(_, Pretty, ""))

  final override def renderCanonical: String = buildString(internalRender(_, Canonical, ""))

  final override def internalRender(sb: StringBuilder, mode: RenderMode, indent: String) {
    mode match {
      case Compact =>
        // render as compactly as possible
        sb.append("[")
        var seen = false
        elements.foreach { v =>
          if (seen) sb.append(",") else seen = true
          v.internalRender(sb, mode, "")
        }
        sb.append("]")

      case _ =>
        val indent2 = indent + "  "
        val oneLine = !isNested
        val delimiter = if (oneLine) ", " else ",\n"

        sb.append("[")
        if (!oneLine) sb.append("\n")
        
        var seen = false
        elements.foreach { v =>
          if (seen) sb.append(delimiter) else seen = true
          if (!oneLine) sb.append(indent2)
          v.internalRender(sb, mode, indent2)
        }

        if (!oneLine) {
          sb.append("\n")
          sb.append(indent)
        }
        sb.append("]")
    }
  }
}

case object JArray extends (List[JValue] => JArray) {
  final val empty = JArray(Nil)
  final implicit val order: Order[JArray] = Order[List[JValue]].contramap((_: JArray).elements)
}
