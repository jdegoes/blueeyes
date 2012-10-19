
package blueeyes.json

/*import scalaz._
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
*/

class Lazy[A] private (f: () => A) extends (() => A) { self =>
  lazy val value: A = f()

  def apply(): A = value

  def map[B](f: A => B): Lazy[B] = Lazy(f(value))

  def flatMap[B](f: A => Lazy[B]): Lazy[B] = Lazy(f(self.value).value)

  override def equals(that: Any): Boolean = that match {
    case that: Lazy[_] => value == that.value
    case _ => false
  }

  override def hashCode() = value.hashCode
}
object Lazy {
  def apply[A](value: => A): Lazy[A] = new Lazy(() => value)

  @inline implicit def LazyToValue[A](thunk: Lazy[A]): A = thunk.value
}

sealed trait JSchema { self =>
  def validate(value: JValue): Boolean

  def | (that: JSchema): JSchema = JEitherSchema(self, that)

  def |+| (that: JSchema): JSchema = (self | that).minimize

  def unfixed: JSchema = minimize(0L)

  def minimize(fixedBound: Long): JSchema = minimize(Some(fixedBound))

  def minimize: JSchema = minimize(None)

  def fuse(that: JSchema): Option[JSchema] = (self, that) match {
    case (x, y) if (x == y) => Some(x)
    
    case (JFixedSchema(_: JBool), JBoolSchema) => Some(JBoolSchema)
    case (JBoolSchema, JFixedSchema(_: JBool)) => Some(JBoolSchema)
    
    case (JFixedSchema(_: JNum), JNumSchema) => Some(JNumSchema)
    case (JNumSchema, JFixedSchema(_: JNum)) => Some(JNumSchema)

    case (JFixedSchema(_: JString), JStringSchema) => Some(JStringSchema)
    case (JStringSchema, JFixedSchema(_: JString)) => Some(JStringSchema)

    case (JFixedSchema(JNull), JNullSchema) => Some(JNullSchema)
    case (JNullSchema, JFixedSchema(JNull)) => Some(JNullSchema)

    case (JFixedSchema(JUndefined), JUndefinedSchema) => Some(JUndefinedSchema)
    case (JUndefinedSchema, JFixedSchema(JUndefined)) => Some(JUndefinedSchema)

    case (JUndefinedSchema, x) => Some(x)
    case (x, JUndefinedSchema) => Some(x)
    
    case (x: JObjectSchema, y: JObjectSchema) =>
      sys.error("TODO")

    case _ => None
  }

  private[json] def minimize(fixedBound: Option[Long]): JSchema
}

case object JSchema {
  lazy val JSON: JSchema = JNullSchema | JBoolSchema | JNumSchema | JStringSchema | JObjectValueSchema(JSON) | JSetSchema(JSON)

  def fixed(value: JValue): JSchema = value match {
    case JArray(elements) =>
      lazy val uniqueEls = Set(elements: _*)

      if (elements.size > 1 && uniqueEls.size == 1) JSetSchema(fixed(elements.head))
      else JArraySchema(Array(elements.map(fixed _): _*))
    
    case JObject(fields) =>
      JObjectSchema(fields.mapValues(fixed _))

    case x => JFixedSchema(value)
  }

  def unfixed(value: JValue): JSchema = value match {
    case JNull => JNullSchema
    
    case JUndefined => JUndefinedSchema
    
    case _ : JBool => JBoolSchema
    
    case _ : JNum => JNumSchema
    
    case _ : JString => JStringSchema

    case JArray(elements) =>
      lazy val elSchemas = elements.map(unfixed _)
      lazy val uniqSchemas = Set(elSchemas: _*)

      if (elements.size == 0) JArraySchema(Array.empty[JSchema])
      else if (elements.size > 1 && uniqSchemas.size == 1) JSetSchema(uniqSchemas.head)
      else JArraySchema(Array(elSchemas: _*))

    case JObject(fields) => JObjectSchema(fields.mapValues(unfixed _))
  }
}

case class JFixedSchema(schema: JValue) extends JSchema {
  def validate(value: JValue): Boolean = (value == schema)

  private[json] def minimize(fixedBound: Option[Long]): JSchema = if (fixedBound.getOrElse(Long.MaxValue) <= 0) JSchema.unfixed(schema) else this
}

case class JObjectValueSchema(private val valueSchema0: Lazy[JSchema]) extends JSchema {
  def valueSchema: JSchema = valueSchema0

  def validate(value: JValue): Boolean = value match {
    case JObject(fields) => fields.values.forall(valueSchema.validate)

    case _ => false
  }

  private[json] def minimize(fixedBound: Option[Long]): JSchema = JObjectValueSchema(valueSchema.minimize(fixedBound))
}
object JObjectValueSchema {
  def apply(valueSchema: => JSchema): JObjectValueSchema = new JObjectValueSchema(Lazy(valueSchema))
}

case class JObjectSchema(private val schema0: Lazy[Map[String, JSchema]]) extends JSchema {
  private lazy val schemaKeysSet = schema.keys.toSet
  private lazy val schemaKeysSeq = schema.keys.toArray
  private lazy val schemaValuesSeq = schemaKeysSeq.map(schema)

  def schema: Map[String, JSchema] = schema0

  def validate(value: JValue): Boolean = {
     value match {
      case JObject(fields) => 
        val fieldKeys = fields.keys

        if (!fieldKeys.forall(schemaKeysSet.contains)) false
        else {
          var ret = true
          var keyIter = fieldKeys.iterator

          while (keyIter.hasNext && ret) {
            val key = keyIter.next()

            ret = ret && schema(key).validate(fields(key))
          }
          ret
        }

      case _ => false
    }
  }

  private[json] def minimize(fixedBound: Option[Long]): JSchema = JObjectSchema(schema.mapValues(_.minimize(fixedBound)))
}
object JObjectSchema {
  def apply(schema: => Map[String, JSchema]): JObjectSchema = new JObjectSchema(Lazy(schema))
}

case class JSetSchema(private val schema0: Lazy[JSchema]) extends JSchema {
  def schema: JSchema = schema0

  def validate(value: JValue): Boolean = value match {
    case JArray(elements) => elements.forall(schema.validate)

    case _ => false
  }

  private[json] def minimize(fixedBound: Option[Long]): JSchema = JSetSchema(schema.minimize)
}
object JSetSchema {
  def apply(schema: => JSchema): JSetSchema = new JSetSchema(Lazy(schema))
}

case class JArraySchema(private val schemas0: Lazy[Array[JSchema]]) extends JSchema {
  def schemas: Seq[JSchema] = schemas0.value

  def validate(value: JValue): Boolean = value match {
    case JArray(elements) => 
      if (elements.length != schemas0.length) false
      else {
        val iter = elements.iterator
        var ret = true
        var i = 0

        while (ret && iter.hasNext) {
          ret = schemas(i).validate(iter.next())
          i = i + 1
        }

        ret
      }

    case _ => false
  }

  private[json] def minimize(fixedBound: Option[Long]): JSchema = JArraySchema(schemas.map(_.minimize(fixedBound)))
}

case object JArraySchema {
  def apply(schemas: => Seq[JSchema]): JArraySchema = new JArraySchema(Lazy(schemas.toArray))
}

case object JNumSchema extends JSchema {
  def validate(value: JValue): Boolean = value match {
    case _ : JNum => true

    case _ => false
  }

  private[json] def minimize(fixedBound: Option[Long]): JSchema = this
}

case object JStringSchema extends JSchema {
  def validate(value: JValue): Boolean = value match {
    case _ : JString => true

    case _ => false
  }

  private[json] def minimize(fixedBound: Option[Long]): JSchema = this
}

case object JNullSchema extends JSchema {
  def validate(value: JValue): Boolean = value match {
    case JNull => true

    case _ => false
  }

  private[json] def minimize(fixedBound: Option[Long]): JSchema = this
}

case object JBoolSchema extends JSchema {
  def validate(value: JValue): Boolean = value match {
    case _ : JBool => true

    case _ => false
  }

  private[json] def minimize(fixedBound: Option[Long]): JSchema = this
}

case object JUndefinedSchema extends JSchema {
  def validate(value: JValue): Boolean = value match {
    case JUndefined => true

    case _ => false
  }

  private[json] def minimize(fixedBound: Option[Long]): JSchema = this
}

case class JEitherSchema(private val left0: Lazy[JSchema], private val right0: Lazy[JSchema]) extends JSchema {
  def left: JSchema = left0

  def right: JSchema = right0

  def validate(value: JValue): Boolean = left.validate(value) || right.validate(value)

  def flatten: Set[JSchema] = {
    def flatten0(cur: JSchema, acc: Set[JSchema]): Set[JSchema] = cur match {
      case JEitherSchema(left, right) => flatten0(right, flatten0(left, acc))
      case x => acc + x
    }

    flatten0(this, Set.empty)
  }

  private[json] def minimize(fixedBound: Option[Long]): JSchema = {
    val disj = flatten.map(_.minimize(fixedBound))

    val fixed = disj.collect { case x : JFixedSchema => x }

    val newDisj = if (fixed.size > fixedBound.getOrElse(Long.MaxValue)) {
      (disj -- fixed) union (fixed.map(s => JSchema.unfixed(s.schema)))
    } else disj

    JEitherSchema(newDisj)
  }
}

case object JEitherSchema {
  def apply(left: => JSchema, right: => JSchema): JEitherSchema = new JEitherSchema(Lazy(left), Lazy(right))

  private[json] def apply(set: Set[JSchema]): JSchema = {
    if (set.isEmpty) JUndefinedSchema
    else if (set.size == 1) set.head
    else set.reduce[JSchema]((a, b) => JEitherSchema(a, b))
  }
}