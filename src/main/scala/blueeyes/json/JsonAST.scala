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

import scalaz.NonEmptyList

object JsonAST {
  import scala.text.{Document, DocText}
  import scala.text.Document._

  /** Concatenates a sequence of <code>JValue</code>s.
   * <p>
   * Example:<pre>
   * concat(JInt(1), JInt(2)) == JArray(List(JInt(1), JInt(2)))
   * </pre>
   */
  def concat(xs: JValue*) = xs.foldLeft(JNothing: JValue)(_ ++ _)

  /**
   * Data type for Json AST.
   */
  sealed abstract class JValue extends Merge.Mergeable with Diff.Diffable with Product with Ordered[JValue] {
    type Values
    type Self <: JValue

    /** Sorts the JValue according to natural ordering. This can be considered a
     * form of "normalization" to aid comparisons between different JValues.
     *
     * {{{
     * json.sort
     * }}}
     */
    def sort: Self = {
      import blueeyes.json.xschema.DefaultOrderings.JValueOrdering

      (this match {
        case JObject(fields)      => JObject(fields.map(_.sort).sorted(JValueOrdering))
        case JArray(elements)     => JArray(elements.map(_.sort).sorted(JValueOrdering))
        case JField(name, value)  => JField(name, value.sort)

        case _ => this
      }).asInstanceOf[Self]
    }

    def compare(that: JValue): Int = blueeyes.json.xschema.DefaultOrderings.JValueOrdering.compare(this, that)

    /** XPath-like expression to query JSON fields by name. Matches only fields on
     * next level.
     * <p>
     * Example:<pre>
     * json \ "name"
     * </pre>
     */
    def \ (nameToFind: String): JValue = {
      def extractValue(jvalue: JValue): JValue = jvalue match {
        case JField(n, v) => v
        case _ => jvalue
      }
      val p = (json: JValue) => json match {
        case JField(name, value) if name == nameToFind => true
        case _ => false
      }
      findDirect(children, p) match {
        case Nil => JNothing
        case x :: Nil => extractValue(x)
        case xs => JArray(xs.map(extractValue))
      }
    }

    def \? (nameToFind: String): Option[JValue] = {
      (this \ nameToFind) match {
        case JNothing | JNull => None
        case x => Some(x)
      }
    }

    /**
     * Returns the element as a JValue of the specified class.
     * <p>
     * Example:<pre>
     * (json \ "foo" --> classOf[JField]).value
     * </pre>
     */
    def --> [A <: JValue](clazz: Class[A]): A = (this -->? clazz).getOrElse(sys.error("Expected class " + clazz + ", but found: " + this.getClass))

    /**
     * Returns the element as an option of a JValue of the specified class.
      * <p>
      * Example:<pre>
      * (json \ "foo" -->? classOf[JField]).map(_.value).getOrElse(defaultFieldValue)
      * </pre>
     */
    def -->? [A <: JValue](clazz: Class[A]): Option[A] = {
      def extractTyped(value: JValue) = if (value.getClass == clazz) Some(value.asInstanceOf[A]) else None

      this match {
        case JField(name, value) if (clazz != classOf[JField]) => extractTyped(value)
        case _ => extractTyped(this)
      }
    }

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
                nextQueue.enqueue(fields.map(_.value))

              case JArray(elements) =>
                nextQueue.enqueue(elements)

              case jvalue => nextQueue
            }
          )
        }
      }

      breadthFirst0(Nil, Queue.empty.enqueue(this)).reverse
    }

    private def findDirect(xs: List[JValue], p: JValue => Boolean): List[JValue] = xs.flatMap {
      case JObject(l) => l.filter {
        case x if p(x) => true
        case _ => false
      }
      case JArray(l) => findDirect(l, p)
      case x if p(x) => x :: Nil
      case _ => Nil
    }

    /** XPath-like expression to query JSON fields by name. Returns all matching fields.
     * <p>
     * Example:<pre>
     * json \\ "name"
     * </pre>
     */
    def \\(nameToFind: String): JValue = {
      def find(json: JValue): List[JField] = json match {
        case JObject(l) => l.foldLeft(List[JField]())((a, e) => a ::: find(e))
        case JArray(l) => l.foldLeft(List[JField]())((a, e) => a ::: find(e))
        case field @ JField(name, value) if name == nameToFind => field :: find(value)
        case JField(_, value) => find(value)
        case _ => Nil
      }
      find(this) match {
        case x :: Nil => x.value
        case x => JArray(x.map(_.value))
      }
    }

    /** XPath-like expression to query JSON fields by type. Matches only fields on
     * next level.
     * <p>
     * Example:<pre>
     * json \ classOf[JInt]
     * </pre>
     */
    def \[A <: JValue](clazz: Class[A]): List[A#Values] =
      findDirect(children, typePredicate(clazz) _).asInstanceOf[List[A]] map { _.values }

    /** XPath-like expression to query JSON fields by type. Returns all matching fields.
     * <p>
     * Example:<pre>
     * json \\ classOf[JInt]
     * </pre>
     */
    def \\[A <: JValue](clazz: Class[A]): List[A#Values] =
      (this filter typePredicate(clazz) _).asInstanceOf[List[A]] map { _.values }

    private def typePredicate[A <: JValue](clazz: Class[A])(json: JValue) = json match {
      case x if x.getClass == clazz => true
      case _ => false
    }

    /** Gets the specified value located at the terminal of the specified path.
     * <p>
     * Example:<pre>
     * json(".foo[0].bar.baz[123]")
     * </pre>
     */
    def apply(path: JPath): JValue = get(path)

    def get(path: JPath): JValue = path.extract(this)

    def set(path: JPath, value: JValue): JValue = if (path == JPath.Identity) value else {
      def up(l: List[JValue], i: Int, v: JValue) = l.length match {
          case len if len == i =>  l :+ v 
          case len if i < 0 || i > len =>
            sys.error("Attempt to create a new element out of JArray bounds at " + i)

          case _ => l.updated(i, v) 
      }

      this match {
        case obj @ JObject(fields) => path.nodes match {
          case JPathField(name) :: Nil => JObject(JField(name, value) :: fields.filterNot(_.name == name))

          case JPathField(name)  :: nodes => JObject(JField(name, (obj \ name).set(JPath(nodes), value)) :: fields.filterNot(_.name == name))
          
          case _ => sys.error("Objects are not indexed")
        }

        case arr @ JArray(elements) => path.nodes match {
          case JPathIndex(index) :: Nil => JArray(up(elements, index, value))
          case JPathIndex(index) :: nodes => JArray(up(elements, index, elements.lift(index).getOrElse(JNothing).set(JPath(nodes), value)))
          case _ => sys.error("Arrays have no fields")
        }

        case _ => path.nodes match {
          case Nil => value

          case JPathIndex(_) :: _ => JArray(Nil).set(path, value)

          case JPathField(_) :: _ => JObject(Nil).set(path, value)
        }
      }
    }

    /** Return nth element from JSON.
     * Meaningful only to JArray, JObject and JField. Returns JNothing for other types.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: Nil)(1) == JInt(2)
     * </pre>
     */
    def apply(i: Int): JValue = JNothing

    /** Return unboxed values from JSON
     * <p>
     * Example:<pre>
     * JObject(JField("name", JString("joe")) :: Nil).values == Map("name" -> "joe")
     * </pre>
     */
    def values: Values

    /** Return direct child elements.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: Nil).children == List(JInt(1), JInt(2))
     * </pre>
     */
    def children: List[JValue] = this match {
      case JObject(l) => l
      case JArray(l) => l
      case JField(n, v) => List(v)
      case _ => Nil
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
          case JObject(l)   => l.foldLeft(newAcc) { rec(_, p, _) }
          case JArray(l)    => l.zipWithIndex.foldLeft(newAcc) { case (a, (e, idx)) => rec(a, p \ idx, e) }
          case JField(n, v) => rec(newAcc, p \ n, v)
          case _ => newAcc
        }
      }

      rec(z, JPath.Identity, this)
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
          case JObject(l)   => l.foldLeft(acc) { (a, f) => rec(a, p, f) }
          case JArray(l)    => l.zipWithIndex.foldLeft(acc) { (a, t) => val (e, idx) = t; rec(a, p \ idx, e) }
          case JField(n, v) => rec(acc, p \ n, v)
          case _ => acc
        }, p, v)
      }

      rec(z, JPath.Identity, this)
    }

    /** Return a new JValue resulting from applying the given function <code>f</code>
     * to each element, moving from the bottom-up.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: Nil) mapUp { case JInt(x) => JInt(x+1); case x => x }
     * </pre>
     */
    def mapUp(f: JValue => JValue): JValue = mapUpWithPath((p, j) => f(j))

    /** Return a new JValue resulting from applying the given function <code>f</code>
     * to each element and its path, moving from the bottom-up.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: Nil) mapUpWithPath { (path, jvalue) => jvalue }
     * </pre>
     */
    def mapUpWithPath(f: (JPath, JValue) => JValue): JValue = {
      def rec(p: JPath, v: JValue): JValue = v match {
        case JObject(l) => f(p, JObject(l.flatMap(f => rec(p, f) match {
          case JNothing => Nil

          case x: JField => x :: Nil

          case x => JField(f.name, x) :: Nil
        })))
        case JArray(l) => f(p, JArray(l.zipWithIndex.flatMap(t => rec(p \ t._2, t._1) match {
          case JNothing => Nil
          case x => x :: Nil
        })))
        case JField(name, value) => rec(p \ name, value) match {
          case JNothing => JNothing
          case x => f(p, JField(name, x))
        }
        case x => f(p, x)
      }
      rec(JPath.Identity, this)
    }

    /** Return a new JValue resulting from applying the given function <code>f</code>
     * to each element, moving from the top-down.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: Nil) mapDown { case JInt(x) => JInt(x+1); case x => x }
     * </pre>
     */
    def mapDown(f: JValue => JValue): JValue = mapDownWithPath((p, j) => f(j))

    /** Return a new JValue resulting from applying the given function <code>f</code>
     * to each element and its path, moving from the top-down.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: Nil) mapDownWithPath { (path, jvalue) => jvalue }
     * </pre>
     */
    def mapDownWithPath(f: (JPath, JValue) => JValue): JValue = {
      def rec(p: JPath, v: JValue): JValue = {
        f(p, v) match {
          case JObject(l) =>
            JObject(l.flatMap { f =>
              rec(p, f) match {
                case JNothing => Nil

                case x: JField => x :: Nil

                case x => JField(f.name, x) :: Nil
              }
            })

          case JArray(l) =>
            JArray(l.zipWithIndex.flatMap { t =>
              val (e, idx) = t

              rec(p \ idx, e) match {
                case JNothing => Nil
                case x => x :: Nil
              }
            })

          case JField(name, value) =>
            rec(p \ name, value) match {
              case JNothing => JNothing
              case x => JField(name, x)
            }

          case x => x
        }
      }

      rec(JPath.Identity, this)
    }

    /** Return a new JValue resulting from applying the given partial function <code>f</code>
     * to each element in JSON.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: Nil) transform { case JInt(x) => JInt(x+1) }
     * </pre>
     */
    def transform(f: PartialFunction[JValue, JValue]): JValue = mapUp { x =>
      if (f.isDefinedAt(x)) f(x) else x
    }

    /** Replaces the matched path values with the result of calling the
     * replacer function on the matches. If the path has no values, the
     * method has no effect -- i.e. it is not an error to specify paths
     * which do not exist.
     * <p>
     * Example:<pre>
     * jvalue.replace(".baz", value => JObject("indent", value))
     * </pre>
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

      target.expand(this).foldLeft(this) { (jvalue, expansion) =>
        replace0(expansion, jvalue)
      }
    }

    /** A shorthand for the other replacement in the case that the replacement
     * does not depend on the value being replaced.
     */
    def replace(target: JPath, replacement: JValue): JValue = replace(target, r => replacement)

    /** Return the first element from JSON which matches the given predicate.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: Nil) find { _ == JInt(2) } == Some(JInt(2))
     * </pre>
     */
    def find(p: JValue => Boolean): Option[JValue] = {
      def find(json: JValue): Option[JValue] = {
        if (p(json)) return Some(json)
        json match {
          case JObject(l) => l.flatMap(find _).headOption
          case JArray(l) => l.flatMap(find _).headOption
          case JField(_, value) => find(value)
          case _ => None
        }
      }
      find(this)
    }

    /** Return a List of all elements which matches the given predicate.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: Nil) filter { case JInt(x) => x > 1; case _ => false }
     * </pre>
     */
    def filter(p: JValue => Boolean): List[JValue] =
      foldDown(List[JValue]())((acc, e) => if (p(e)) e :: acc else acc).reverse

    def flatten: List[JValue] =
      foldDown(List[JValue]())((acc, e) => e :: acc).reverse

    /** Flattens the JValue down to a list of path to simple JValue primitive.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: Nil) flattenWithPath
     * </pre>
     */
    def flattenWithPath: List[(JPath, JValue)] = {
      def flatten0(path: JPath)(value: JValue): List[(JPath, JValue)] = value match {
        case JObject(fields) => fields.flatMap(flatten0(path))

        case JArray(elements) => elements.zipWithIndex.flatMap { tuple =>
          val (element, index) = tuple

          flatten0(path \ index)(element)
        }

        case JField(name, value) => flatten0(path \ name)(value)

        case _ => (path -> value) :: Nil
      }

      flatten0(JPath.Identity)(this)
    }

    /** Concatenate with another JSON.
     * This is a concatenation monoid: (JValue, ++, JNothing)
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: Nil) ++ JArray(JInt(3) :: Nil) ==
     * JArray(List(JInt(1), JInt(2), JInt(3)))
     * </pre>
     */
    def ++(other: JValue) = {
      def append(value1: JValue, value2: JValue): JValue = (value1, value2) match {
        case (JNothing, x) => x
        case (x, JNothing) => x
        case (JObject(xs), x: JField) => JObject(xs ::: List(x))
        case (x: JField, JObject(xs)) => JObject(x :: xs)
        case (JArray(xs), JArray(ys)) => JArray(xs ::: ys)
        case (JArray(xs), v: JValue) => JArray(xs ::: List(v))
        case (v: JValue, JArray(xs)) => JArray(v :: xs)
        case (f1: JField, f2: JField) => JObject(f1 :: f2 :: Nil)
        case (JField(n, v1), v2: JValue) => JField(n, append(v1, v2))
        case (x, y) => JArray(x :: y :: Nil)
      }
      append(this, other)
    }

    /** Return a JSON where all elements matching the given predicate are removed.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: JNull :: Nil) remove { _ == JNull }
     * </pre>
     */
    def remove(p: JValue => Boolean): JValue = this mapUp {
      case x if p(x) => JNothing
      case x => x
    }
  }

  case object JNothing extends JValue {
    type Values = None.type
    type Self = JValue

    def values = None
  }
  case object JNull extends JValue {
    type Values = Null
    type Self = JValue
    
    def values = null
  }
  case class JBool(value: Boolean) extends JValue {
    type Values = Boolean
    type Self = JValue
    
    def values = value
  }
  case class JInt(value: BigInt) extends JValue {
    type Values = BigInt
    type Self = JValue
    
    def values = value
  }
  case class JDouble(value: Double) extends JValue {
    type Values = Double
    type Self = JValue
    
    def values = value
  }
  case class JString(value: String) extends JValue {
    type Values = String
    type Self = JValue
    
    def values = value
  }
  case class JField(name: String, value: JValue) extends JValue {
    type Values = (String, value.Values)
    type Self = JField
    
    def values = (name, value.values)
    override def apply(i: Int): JValue = value(i)
  }
  case class JObject(fields: List[JField]) extends JValue {
    type Values = Map[String, Any]
    type Self = JObject
    
    def values = Map() ++ fields.map(_.values : (String, Any))

    override lazy val hashCode = Set(this.fields: _*).hashCode

    override def equals(that: Any): Boolean = that match {
      case that: JObject if (this.fields.length == that.fields.length) => Set(this.fields: _*) == Set(that.fields: _*)
      case _ => false
    }
  }
  object JObject {
    lazy val empty = JObject(Nil)
  }
  case class JArray(elements: List[JValue]) extends JValue {
    type Values = List[Any]
    type Self = JArray
    
    def values = elements.map(_.values)

    override def apply(i: Int): JValue = elements.lift(i).getOrElse(JNothing)
  }
  object JArray {
    lazy val empty = JArray(Nil)
  }

}

/** Basic implicit conversions from primitive types into JSON.
 * Example:<pre>
 * import blueeyes.json.Implicits._
 * JObject(JField("name", "joe") :: Nil) == JObject(JField("name", JString("joe")) :: Nil)
 * </pre>
 */
object Implicits extends Implicits
trait Implicits {
  import JsonAST._

  implicit def int2jvalue(x: Int) = JInt(x)
  implicit def long2jvalue(x: Long) = JInt(x)
  implicit def bigint2jvalue(x: BigInt) = JInt(x)
  implicit def double2jvalue(x: Double) = JDouble(x)
  implicit def float2jvalue(x: Float) = JDouble(x)
  implicit def bigdecimal2jvalue(x: BigDecimal) = JDouble(x.doubleValue)
  implicit def boolean2jvalue(x: Boolean) = JBool(x)
  implicit def string2jvalue(x: String) = JString(x)
}

/** A DSL to produce valid JSON.
 * Example:<pre>
 * import blueeyes.json.JsonDSL._
 * ("name", "joe") ~ ("age", 15) == JObject(JField("name",JString("joe")) :: JField("age",JInt(15)) :: Nil)
 * </pre>
 */
object JsonDSL extends JsonDSL with Printer
trait JsonDSL extends Implicits {
  import JsonAST._

  implicit def seq2jvalue[A <% JValue](s: Seq[A]) = JArray(s.toList.map { a => a: JValue })
  implicit def nel2JValue[A <% JValue](s: NonEmptyList[A]) = JArray(s.list.map { a => a: JValue })
  implicit def option2jvalue[A <% JValue](opt: Option[A]): JValue = opt match {
    case Some(x) => x
    case None => JNothing
  }

  implicit def symbol2jvalue(x: Symbol) = JString(x.name)
  implicit def pair2jvalue[A <% JValue](t: (String, A)) = JObject(List(JField(t._1, t._2)))
  implicit def list2jvalue(l: List[JField]) = JObject(l)
  implicit def jobject2assoc(o: JObject) = new JsonListAssoc(o.fields)
  implicit def pair2Assoc[A <% JValue](t: (String, A)) = new JsonAssoc(t)

  class JsonAssoc[A <% JValue](left: (String, A)) {
    def ~[B <% JValue](right: (String, B)) = {
      val l: JValue = left._2
      val r: JValue = right._2
      JObject(JField(left._1, l) :: JField(right._1, r) :: Nil)
    }

    def ~(right: JObject) = {
      val l: JValue = left._2
      JObject(JField(left._1, l) :: right.fields)
    }
  }

  class JsonListAssoc(left: List[JField]) {
    def ~(right: (String, JValue)) = JObject(left ::: List(JField(right._1, right._2)))
    def ~(right: JObject) = JObject(left ::: right.fields)
  }
}

/** Printer converts JSON to String.
 * Before printing a <code>JValue</code> needs to be rendered into scala.text.Document.
 * <p>
 * Example:<pre>
 * pretty(render(json))
 * </pre>
 *
 * @see blueeyes.json.JsonAST#render
 */
object Printer extends Printer
trait Printer {
  import java.io._
  import java.util.IdentityHashMap
  import scala.text.{Document, DocText, DocCons, DocBreak, DocNest, DocGroup, DocNil}
  import scala.text.Document._
  import scala.collection.immutable.Stack
  import JsonAST._

  /** Renders JSON.
   * @see Printer#compact
   * @see Printer#pretty
   */
  def render(value: JValue): Document = value match {
    case null          => text("null")
    case JBool(true)   => text("true")
    case JBool(false)  => text("false")
    case JDouble(n)    => text(n.toString)
    case JInt(n)       => text(n.toString)
    case JNull         => text("null")
    case JNothing      => sys.error("can't render 'nothing'")
    case JString(null) => text("null")
    case JString(s)    => text("\"" + quote(s) + "\"")
    case JArray(elements) => text("[") :: series(trimArr(elements).map(render)) :: text("]")
    case JField(n, v)  => text("\"" + quote(n) + "\":") :: render(v)
    case JObject(obj)  =>
      val nested = break :: fields(trimObj(obj).map(render _))
      text("{") :: nest(2, nested) :: break :: text("}")
  }

  /** Renders as Scala code, which can be copy/pasted into a lift-json scala
   * application.
   */
  def renderScala(value: JValue): Document = {
    val Quote = "\""

    def scalaQuote(s: String) = Quote + List("\\t" -> "\\t", "\\f" -> "\\f", "\\r" -> "\\r", "\\n" -> "\\n", "\\\\" -> "\\\\").foldLeft(s) { (str, pair) =>
      str.replaceAll(pair._1, pair._2)
    } + Quote

    def intersperse(l: List[Document], i: Document) = l.zip(List.fill(l.length - 1)({i}) ::: List(text(""))).map(t => t._1 :: t._2)

    value match {
      case null => text("null")

      case JNothing => text("JNothing")
      case JNull => text("JNull")

      case _ => text(value.productPrefix + "(") :: (value match {
        case JNull | JNothing => sys.error("impossible")

        case JBool(value)  => text(value.toString)
        case JDouble(n)    => text(n.toString)
        case JInt(n)       => text(n.toString)
        case JString(null) => text("null")
        case JString(s)    => text(scalaQuote(s))
        case JArray(elements)   => fold(intersperse(elements.map(renderScala) ::: List(text("Nil")), text("::")))
        case JField(n, v)  => text(scalaQuote(n) + ",") :: renderScala(v)
        case JObject(obj)  =>
          val nested = break :: fold(intersperse(intersperse(obj.map(renderScala) ::: List(text("Nil")), text("::")), break))

          nest(2, nested)
      }) :: text(")")
    }
  }

  private def trimArr(xs: List[JValue]) = xs.filter(_ != JNothing)
  private def trimObj(xs: List[JField]) = xs.filter(_.value != JNothing)
  private def series(docs: List[Document]) = fold(punctuate(text(","), docs))
  private def fields(docs: List[Document]) = fold(punctuate(text(",") :: break, docs))
  private def fold(docs: List[Document]) = docs.foldLeft[Document](empty)(_ :: _)

  private def punctuate(p: => Document, docs: List[Document]): List[Document] = {
    def prepend(d: DocText, ds: List[Document]) = ds match {
      case DocText(h) :: t => DocText(h + d.txt) :: t
      case _ => d :: ds
    }

    def punctuate0(docs: List[Document], acc: List[Document]): List[Document] = docs match {
      case Nil => acc.reverse
      case List(d) => punctuate0(Nil, d :: acc)
      case DocText(d) :: ds => p match {
        case DocText(punct) => punctuate0(ds, prepend(DocText(d + punct), acc))
        case _ => punctuate0(ds, (d :: p) :: acc)
      }
      case d :: ds => punctuate0(ds, (d :: p) :: acc)
    }
    punctuate0(docs, Nil)
  }

  private[json] def quote(s: String): String = {
    val buf = new StringBuilder
    for (i <- 0 until s.length) {
      val c = s.charAt(i)
      buf.append(c match {
        case '"'  => "\\\""
        case '\\' => "\\\\"
        case '\b' => "\\b"
        case '\f' => "\\f"
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\t' => "\\t"
        case c if ((c >= '\u0000' && c < '\u001f') || (c >= '\u0080' && c < '\u00a0') || (c >= '\u2000' && c < '\u2100')) => "\\u%04x".format(c: Int)
        case c => c
      })
    }
    buf.toString
  }

  /** Compact printing (no whitespace etc.)
   */
  def compact(d: Document): String = compact(d, new StringWriter).toString

  /** Compact printing (no whitespace etc.)
   */
  def compact[A <: Writer](d: Document, out: A): A = {
    // Non-recursive implementation to support serialization of big structures.
    var nodes = Stack.empty.push(d)
    val visited = new IdentityHashMap[Document, Unit]()
    while (!nodes.isEmpty) {
      val cur = nodes.top
      nodes = nodes.pop
      cur match {
        case DocText(s)      => out.write(s)
        case DocCons(d1, d2) =>
          if (!visited.containsKey(cur)) {
            visited.put(cur, ())
            nodes = nodes.push(cur)
            nodes = nodes.push(d1)
          } else {
            nodes = nodes.push(d2)
          }
        case DocBreak        =>
        case DocNest(_, d)   => nodes = nodes.push(d)
        case DocGroup(d)     => nodes = nodes.push(d)
        case DocNil          =>
      }
    }
    out.flush
    out
  }

  /** Pretty printing.
   */
  def pretty(d: Document): String = pretty(d, new StringWriter).toString

  /** Pretty printing.
   */
  def pretty[A <: Writer](d: Document, out: A): A = {
    d.format(0, out)
    out
  }

  def normalize[T <: JValue](jvalue: T): T = {
    import blueeyes.json.xschema.DefaultOrderings.JValueOrdering

    jvalue match {
      case JObject(fields) => JObject(fields.map(normalize _).sorted(JValueOrdering)).asInstanceOf[T]
      case JArray(elements) => JArray(elements.map(normalize _).sorted(JValueOrdering)).asInstanceOf[T]
      case JField(name, value) => JField(name, normalize(value)).asInstanceOf[T]

      case _ => jvalue
    }
  }

  /** Renders a normalized JValue.
   */
  def renderNormalized(jvalue: JValue): String = compact(render(normalize(jvalue)))
}
