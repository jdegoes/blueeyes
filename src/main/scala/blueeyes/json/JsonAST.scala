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

import blueeyes.json.xschema.DefaultOrderings._

import scala.annotation.tailrec
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
   * Typesafe casting for jvalues via a partial function. This is here mostly to help
   * out type inference in the JManifest implementations
   */
  def cast[A <: JValue](jvalue: JValue, pf: PartialFunction[JValue, A]) = pf.lift.apply(jvalue)

  /**
   * Data type for Json AST.
   */
  sealed abstract class JValue extends Product with Merge.Mergeable with Diff.Diffable {
    type Unboxed

    /**
     * Returns an unboxed primitive representation of this data
     */
    def unbox: Unboxed

    /** Sorts the JValue according to natural ordering. This can be considered a
     * form of "normalization" to aid comparisons between different JValues.
     *
     * {{{
     * json.sort
     * }}}
     */
    def sort: JValue

    def compare(that: JValue): Int = blueeyes.json.xschema.DefaultOrderings.JValueOrdering.compare(this, that)

    /** XPath-like expression to query JSON fields by name. Matches only fields on
     * next level.
     * <p>
     * Example:<pre>
     * json \ "name"
     * </pre>
     */
    def \ (nameToFind: String): JValue = this match {
      case JObject(fields) => fields.find(_.name == nameToFind).map(_.value).getOrElse(JNothing)
      case _ => JNothing
    }

    def \? (nameToFind: String): Option[JValue] = {
      (this \ nameToFind) match {
        case JNothing | JNull => None
        case x => Some(x)
      }
    }

    /**
     * Returns the element as an option of a JValue of the specified type.
      * <p>
      * Example:<pre>
      * (json \ "foo" as JString).map(_.value).getOrElse(defaultFieldValue)
      * </pre>
     */
    def as[A <: JValue](implicit m : JManifest{type JType = A}): Option[A] = m(this)

    def asUnsafe[A <: JValue](implicit m: JManifest{type JType = A}): A = m(this).getOrElse {
      sys.error("Expected " + m + " but got " + this)
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

    /** XPath-like expression to query JSON fields by name. Returns all matching fields.
     * <p>
     * Example:<pre>
     * json \\ "name"
     * </pre>
     */
    def \\(nameToFind: String): JValue = {
      def find(json: JValue, acc: List[JField]): List[JField] = json match {
        case JObject(l) => l.foldLeft(acc) { (a, f) => 
          if (f.name == nameToFind) f :: find(f.value, a)
          else find(f.value, a)
        }

        case JArray(l) => l.foldLeft(acc) { (a, v) => find(v, a) }

        case _ => acc
      }

      JArray(find(this, Nil).map(_.value).reverse)
    }

    /** XPath-like expression to query JSON fields by type. Matches only fields on
     * next level.
     * <p>
     * Example:<pre>
     * json \ classOf[JInt]
     * </pre>
     */
    def \[A <: JValue](implicit m: JManifest{type JType = A}): List[A] = this match {
      case JArray(values)  => values.flatMap(m(_))
      case JObject(fields) => fields.flatMap(f => m(f.value))
      case _ => Nil
    }

    def \[A <: JValue](clazz: Class[A])(implicit m: JManifest{type JType = A}): List[A] = this \ m
      
    /** XPath-like expression to query JSON fields by type. Returns all matching fields.
     * <p>
     * Example:<pre>
     * json \\ classOf[JInt]
     * </pre>
     */
    def \\[A <: JValue](implicit m: JManifest{type JType = A}): List[A] = m(this).toList ::: (
      this match {
        case JArray(values)  => values.flatMap(_ \\ m)  
        case JObject(fields) => fields.map(_.value).flatMap(_ \\ m)
        case _ => Nil
      }
    )

    // legacy syntax support
    def \\[A <: JValue](clazz: Class[A])(implicit m: JManifest{type JType = A}): List[A] = this \\ m

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
          case JObject(l)   => l.foldLeft(newAcc) { 
            case (a, JField(name, value)) => rec(a, p \ name, value) 
          }

          case JArray(l)    => l.zipWithIndex.foldLeft(newAcc) { 
            case (a, (e, idx)) => rec(a, p \ idx, e) 
          }

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
        f(
          v match {
            case JObject(l)   => l.foldLeft(acc) { (a, f) => rec(a, p \ f.name, f.value) }

            case JArray(l)    => l.zipWithIndex.foldLeft(acc) { 
              case (a, (e, idx)) => rec(a, p \ idx, e) 
            }

            case _ => acc
          }, 
          p, v
        )
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
        case JObject(l) => f(p, JObject(l.flatMap(field => rec(p, field.value) match {
          case JNothing => Nil
          case x => JField(field.name, x) :: Nil
        })))

        case JArray(l) => f(p, JArray(l.zipWithIndex.flatMap(t => rec(p \ t._2, t._1) match {
          case JNothing => Nil
          case x => x :: Nil
        })))

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
            JObject(l.flatMap { 
              f => rec(p, f.value) match {
                case JNothing => Nil
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

    /** 
     * Return the first element from JSON which matches the given predicate using a depth-first search.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: Nil) find { _ == JInt(2) } == Some(JInt(2))
     * </pre>
     */
    def find(p: JValue => Boolean): Option[JValue] = {
      def find(values: List[JValue]): Option[JValue] = values match {
        case x :: xs => x.find(p).orElse(find(xs)) //depth first search
        case Nil => None
      }

      this match {
        case x if p(x)  => Some(x)
        case JObject(l) => find(l.map(_.value))
        case JArray(l)  => find(l)
        case _          => None
      }
    }

    /** Return a List of all elements which matches the given predicate.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: Nil) filter { case JInt(x) =&gt; x &gt; 1; case _ =&gt; false }
     * </pre>
     */
    def filter(p: JValue => Boolean): List[JValue] =
      foldDown(List.empty[JValue])((acc, e) => if (p(e)) e :: acc else acc).reverse

    def flatten: List[JValue] =
      foldDown(List.empty[JValue])((acc, e) => e :: acc).reverse

    /** Flattens the JValue down to a list of path to simple JValue primitive.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: Nil) flattenWithPath
     * </pre>
     */
    def flattenWithPath: List[(JPath, JValue)] = {
      def flatten0(path: JPath)(value: JValue): List[(JPath, JValue)] = value match {
        case JObject(fields) => fields.map(_.value).flatMap(flatten0(path))

        case JArray(elements) => elements.zipWithIndex.flatMap { 
          case (element, index) => flatten0(path \ index)(element)
        }

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
    def ++(other: JValue) = (this, other) match {
      case (JNothing, x) => x
      case (x, JNothing) => x
      case (JArray(xs), JArray(ys)) => JArray(xs ::: ys)
      case (JArray(xs), v: JValue)  => JArray(xs :+ v)
      case (v: JValue, JArray(xs))  => JArray(v :: xs)
      case (x, y) => JArray(x :: y :: Nil)
    }

    /** Return a JSON where all elements matching the given predicate are removed.
     * <p>
     * Example:<pre>
     * JArray(JInt(1) :: JInt(2) :: JNull :: Nil) remove { _ == JNull }
     * </pre>
     */
    def remove(p: JValue => Boolean): JValue = this mapUp {
      x => if (p(x)) JNothing else x
    }

    def removeField(f: JField => Boolean): JValue = this mapUp {
      case JObject(fields) => JObject(fields.filter(v => !f(v)))
      case x => x
    }
  }

  sealed trait JManifest {
    type JType <: JValue
    def apply(v: JValue): Option[JType] 
  }

  object JManifest {
    implicit val JNothingM = JNothing
    implicit val JNullM    = JNull
    implicit val JBoolM    = JBool
    implicit val JIntM     = JInt
    implicit val JDoubleM  = JDouble
    implicit val JStringM  = JString
    implicit val JArrayM   = JArray
    implicit val JObjectM  = JObject
  }

  case object JNothing extends JValue with JManifest {
    type Unboxed = Nothing
    override def unbox = sys.error("Cannot unbox JNothing")
    override def sort: JNothing.type = this

    type JType = JNothing.type
    override def apply(v: JValue) = cast[JNothing.type](v, {case JNothing => JNothing})
  }

  case object JNull extends JValue with JManifest{
    type Unboxed = None.type
    override def unbox = None
    override def sort: JNull.type = this

    type JType = JNull.type
    override def apply(v: JValue) = cast[JNull.type](v, {case JNull => JNull})
  }

  case class JBool(value: Boolean) extends JValue {
    type Unboxed = Boolean
    override def unbox = value
    override def sort: JBool = this
  }
  case object JBool extends JManifest {
    type JType = JBool
    override def apply(v: JValue) = cast(v, {case v: JBool => v})
  }

  case class JInt(value: BigInt) extends JValue {
    type Unboxed = BigInt
    override def unbox = value
    override def sort: JInt = this  
  }
  case object JInt extends JManifest {
    type JType = JInt
    override def apply(v: JValue) = cast(v, {case v: JInt => v})
  }

  case class JDouble(value: Double) extends JValue {
    type Unboxed = Double
    override def unbox = value
    override def sort: JDouble = this
  }
  case object JDouble extends JManifest {
    type JType = JDouble 
    override def apply(v: JValue) = cast(v, {case v: JDouble => v})
  }

  case class JString(value: String) extends JValue {
    type Unboxed = String
    override def unbox = value
    override def sort: JString = this
  }
  case object JString extends JManifest {
    type JType = JString
    override def apply(v: JValue) = cast(v, {case v: JString => v})
  }

  case class JArray(elements: List[JValue]) extends JValue {
    type Unboxed = List[Any]
    override lazy val unbox = elements.map(_.unbox)
    override def sort: JArray = JArray(elements.map(_.sort).sorted(JValueOrdering))
    override def apply(i: Int): JValue = elements.lift(i).getOrElse(JNothing)
  }
  case object JArray extends JManifest {
    type JType = JArray
    override def apply(v: JValue) = cast(v, {case v: JArray => v})
    lazy val empty = JArray(Nil)
  }

  case class JObject(fields: List[JField]) extends JValue {
    type Unboxed = Map[String, Any]
    override lazy val unbox = fields.map[(String, Any), Map[String, Any]](f => (f.name, f.value.unbox))(collection.breakOut)
    override def sort: JObject = JObject(fields.map(_.sort).sorted(JFieldOrdering))

    override lazy val hashCode = Set(this.fields: _*).hashCode
    override def equals(that: Any): Boolean = that match {
      case JObject(fields) if (this.fields.length == fields.length) => 
        Set(this.fields: _*) == Set(fields: _*)

      case _ => false
    }
  }
  case object JObject extends JManifest {
    type JType = JObject
    override def apply(v: JValue) = cast(v, {case v: JObject => v})
    lazy val empty = JObject(Nil)
  }

  case class JField(name: String, value: JValue) {
    def sort: JField = JField(name, value.sort)
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
  import JsonAST._

  import scala.text.Document._
  import scala.text.{Document, DocText, DocCons, DocBreak, DocNest, DocGroup, DocNil}
  import scala.collection.immutable.Stack

  import java.io._
  import java.util.IdentityHashMap

  /** Renders JSON.
   * @see Printer#compact
   * @see Printer#pretty
   */
  def render(value: JValue): Document = {
    def renderField(field: JField): Document = text("\"" + quote(field.name) + "\":") :: render(field.value)

    value match {
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
      case JObject(obj)  =>
        val nested = break :: fields(trimObj(obj).map(renderField))
        text("{") :: nest(2, nested) :: break :: text("}")
    }
  }


  /** Renders as Scala code, which can be copy/pasted into a lift-json scala
   * application.
   */
  def renderScala(value: JValue): Document = {
    val Quote = "\""
    val Escaped = List("\\t" -> "\\t", "\\f" -> "\\f", "\\r" -> "\\r", "\\n" -> "\\n", "\\\\" -> "\\\\")

    def scalaQuote(s: String) = Quote + (Escaped.foldLeft(s) { case (str, (a, b)) => str.replaceAll(a, b) }) + Quote

    @tailrec def intersperse(l: List[Document], i: Document): Document = l match {
      case x :: y :: xs => intersperse((x :: i :: y) :: xs, i)
      case x :: Nil => x
      case Nil => DocNil
    }
    
    def renderScalaField(f: JField) = text("JField") :: text("(") :: text(scalaQuote(f.name)) :: text(",") :: renderScala(f.value) :: text(")")

    def t(p: Product, doc: Document) = text(p.productPrefix) :: text("(") :: doc :: text(")")

    value match {
      case null => text("null")
      case JNothing         => text("JNothing")
      case JNull            => text("JNull")
      case JBool(b)         => t(value, text(b.toString))
      case JDouble(n)       => t(value, text(n.toString))
      case JInt(n)          => t(value, text(n.toString))
      case JString(null)    => t(value, text("null"))
      case JString(s)       => t(value, text(scalaQuote(s)))
      case JArray(elements) => t(value, intersperse(elements.map(renderScala) ::: List(text("Nil")), text("::")))
      case JObject(fields)  => t(value, nest(2, break :: intersperse(fields.map(renderScalaField) ::: List(text("Nil")), text("::") :: break)))
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

  def compact[A <: Writer](d: Document, out: A): A = { 
    // Non-recursive implementation to support serialization of big structures.
    // careful - this will break if you reuse any components of the document tree
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

  def normalize(jvalue: JValue): JValue = {
    import blueeyes.json.xschema.DefaultOrderings.JValueOrdering

    jvalue match {
      case JObject(fields) => JObject(fields.map(f => JField(f.name, normalize(f.value))).sorted(JFieldOrdering))
      case JArray(elements) => JArray(elements.map(normalize _).sorted(JValueOrdering))
      case _ => jvalue
    }
  }

  /** Renders a normalized JValue.
   */
  def renderNormalized(jvalue: JValue): String = compact(render(normalize(jvalue)))
}
