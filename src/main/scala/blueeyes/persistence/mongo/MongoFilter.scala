package blueeyes.persistence.mongo

import scala.collection.immutable.ListSet
import blueeyes.json.{JPath, JPathIndex, JPathField}
import blueeyes.json.JsonAST._
import blueeyes.util.ProductPrefixUnmangler

import scalaz._
import Scalaz._

object MongoFilterOperators {
  sealed trait MongoFilterOperator extends Product with ProductPrefixUnmangler {
    def symbol: String = unmangledName

    def unary_! : MongoFilterOperator

    override def toString = symbol
  }
  sealed trait MongoFilterOperatorBound extends MongoFilterOperator
  case object $gt   extends MongoFilterOperatorBound  { def unary_! = $lte; }
  case object $gte  extends MongoFilterOperatorBound  { def unary_! = $lt; }
  case object $lt   extends MongoFilterOperatorBound  { def unary_! = $gte; }
  case object $lte  extends MongoFilterOperatorBound  { def unary_! = $gt; }

  sealed trait MongoFilterOperatorEquality extends MongoFilterOperator
  case object $eq extends MongoFilterOperatorEquality { def unary_! = $ne; } // This is a virtual operator, it's not real!!!!
  case object $ne extends MongoFilterOperatorEquality { def unary_! = $eq; }

  sealed trait MongoFilterOperatorContainment extends MongoFilterOperator
  case object $in   extends MongoFilterOperatorContainment { def unary_! = $nin; }
  case object $nin  extends MongoFilterOperatorContainment { def unary_! = $in; }

  case object $mod        extends MongoFilterOperator { def unary_! : MongoFilterOperator = sys.error("The $mod operator does not have a negation"); }
  case object $all        extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $all operator does not have a negation"); }
  case object $size       extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $size operator does not have a negation"); }
  case object $exists     extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $exists operator does not have a negation"); }
  case object $type       extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $type operator does not have a negation"); }
  case object $or         extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $or operator does not have a negation"); }
  case object $each       extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $each operator does not have a negation"); }
}

import MongoFilterOperators._

sealed trait MongoFilter { self =>
  def filter: JValue

  def unary_! : MongoFilter

  def & (that: MongoFilter)  = &&(that)

  def && (that: MongoFilter) = MongoFilterAndMonoid.append(self, that)

  def | (that: MongoFilter)  = ||(that)

  def || (that: MongoFilter) = MongoFilterOrMonoid.append(self, that)
}

object MongoFilter extends MongoFilterImplicits {
  def apply(lhs: JPath, operator: MongoFilterOperator, rhs: MongoPrimitive): MongoFilter = rhs match {
    case opt @ MongoPrimitiveOption(value) => value.map(MongoFieldFilter(lhs, operator, _)).getOrElse(MongoFilterAll)
    case rhs => MongoFieldFilter(lhs, operator, rhs)
  }
}

case object MongoFilterAll extends MongoFilter {
  def filter: JValue = JObject(Nil)

  def unary_! : MongoFilter = this
}

sealed case class MongoFieldFilter(lhs: JPath, operator: MongoFilterOperator, rhs: MongoPrimitive) extends MongoFilter { self =>
  def filter: JValue = {
    val value = operator match {
      case $eq => rhs.toJValue

      case _ => JObject(JField(operator.symbol, rhs.toJValue) :: Nil)
    }
    lhs.nodes match {
      case Nil => value
      case _   => JObject(JField(JPathExtension.toMongoField(lhs), value) :: Nil)
    }
  }

  def unary_! : MongoFilter = MongoFieldFilter(lhs, !operator, rhs)
}

sealed case class MongoOrFilter(queries: ListSet[MongoFilter]) extends MongoFilter {
  def filter: JValue = JObject(JField($or.symbol, JArray(queries.toList.map(_.filter))) :: Nil)

  def unary_! : MongoFilter = MongoAndFilter(queries.map(!_))
}

sealed case class MongoAndFilter(queries: ListSet[MongoFilter]) extends MongoFilter { self =>
  def filter: JValue = {
    val (notEqs, eqs) = queries partition { filter => filter match {
      case MongoFieldFilter(lhs, e @ $eq, rhs) => false
      case _ => true
    }}
    JObject(notEqsQuery(notEqs).fields ::: eqsQuery(eqs).fields)
  }

  private def notEqsQuery(queries: ListSet[MongoFilter]) = {
    import blueeyes.json.MergeMonoid
    val objects = queries.map(_.filter).toList
    (JObject(Nil) :: objects).asMA.sum.asInstanceOf[JObject]
  }
  private def eqsQuery(queries: ListSet[MongoFilter]) = {
    import blueeyes.json.ConcatMonoid
    val fields = queries.map(_.asInstanceOf[MongoFieldFilter]).map(v => JField(JPathExtension.toMongoField(v.lhs), v.rhs.toJValue).asInstanceOf[JValue]).toList
    (JObject(Nil) :: fields).asMA.sum.asInstanceOf[JObject]
  }

  def unary_! : MongoFilter = MongoOrFilter(queries.map(!_))

  def elemMatch(path: JPath) = MongoElementsMatchFilter(path, self)
}

sealed case class MongoElementsMatchFilter(lhs: JPath, elementsQuery: MongoAndFilter) extends MongoFilter{
  def unary_! = sys.error("The $elemMatch operator does not have a negation")

  def filter = {
    val value = JObject(JField("$elemMatch", elementsQuery.filter) :: Nil)
    lhs.nodes match{
      case Nil => value
      case _   => JObject(JField(JPathExtension.toMongoField(lhs), value) :: Nil)
    }
  }
}

sealed trait MongoPrimitive {
  def toJValue: JValue
}

case class MongoPrimitiveOption(value: Option[MongoPrimitive]) extends MongoPrimitive {
  def toJValue = value.map(_.toJValue).getOrElse(JNothing)
}
case class MongoPrimitiveString(value: String) extends MongoPrimitive {
  def toJValue = JString(value)
}
case class MongoPrimitiveLong(value: Long) extends MongoPrimitive {
  def toJValue = JInt(value)
}
case class MongoPrimitiveInt(value: Int) extends MongoPrimitive {
  def toJValue = JInt(value)
}
case class MongoPrimitiveDouble(value: Double) extends MongoPrimitive {
  def toJValue = JDouble(value)
}
case class MongoPrimitiveBoolean(value: Boolean) extends MongoPrimitive {
  def toJValue = JBool(value)
}
case class MongoPrimitiveArray(value: List[MongoPrimitive]) extends MongoPrimitive {
  def toJValue = JArray(value.map(_.toJValue))
}
case class MongoPrimitiveJObject(value: JObject) extends MongoPrimitive {
  def toJValue = value
}
case object MongoPrimitiveNull extends MongoPrimitive {
  def toJValue = JNull
}

sealed class MongoPrimitiveWitness[T](val typeNumber: Int)

trait MongoFilterImplicits {
  import MongoFilterOperators._

  implicit def mongoOperatorToSymbolString(op: MongoFilterOperator): String = op.symbol

  implicit def stringToMongoFilterBuilder(string: String): MongoFilterBuilder = MongoFilterBuilder(JPath(string))

  implicit def jpathToMongoFilterBuilder(jpath: JPath): MongoFilterBuilder = MongoFilterBuilder(jpath)

  implicit def jvalueToMongoPrimitive(value: JValue): MongoPrimitive = value match {
    case x: JString => MongoPrimitiveString(x.value)
    case x: JInt    => MongoPrimitiveLong(x.value.longValue)
    case x: JDouble => MongoPrimitiveDouble(x.value)
    case x: JBool   => MongoPrimitiveBoolean(x.value)
    case x: JObject => MongoPrimitiveJObject(x)
    case x: JArray  => MongoPrimitiveArray(x.elements.map(jvalueToMongoPrimitive))
    case JNull | JNothing => MongoPrimitiveNull
    case JField(_, _) => sys.error("Cannot convert JField to Mongo primitive")
  }

  implicit def optionToMongoPrimitiveOption(value: Option[MongoPrimitive]) = MongoPrimitiveOption(value)
  implicit def stringToMongoPrimitiveString(value: String)    = MongoPrimitiveString(value)
  implicit def longToMongoPrimitiveLong(value: Long)          = MongoPrimitiveLong(value)
  implicit def intToMongoPrimitiveInt(value: Int)             = MongoPrimitiveInt(value)
  implicit def doubleToMongoPrimitiveDouble(value: Double)    = MongoPrimitiveDouble(value)
  implicit def booleanToMongoPrimitiveBoolean(value: Boolean) = MongoPrimitiveBoolean(value)
  implicit def arrayToMongoPrimitiveArray(value: List[MongoPrimitive]) = MongoPrimitiveArray(value)

  /*
  Double	 1
  String	 2
  Object	 3
  Array	 4
  Binary data	 5
  Object id	 7
  Boolean	 8
  Date	 9
  Null	 10
  Regular expression	 11
  JavaScript code	 13
  Symbol	 14
  JavaScript code with scope	 15
  32-bit integer	 16
  Timestamp	 17
  64-bit integer	 18
  Min key	 255
  Max key	 127
  */
  implicit val MongoPrimitiveJStringWitness = new MongoPrimitiveWitness[JString](2)
  implicit val MongoPrimitiveJDoubleWitness = new MongoPrimitiveWitness[JDouble](1)
  implicit val MongoPrimitiveJObjectWitness = new MongoPrimitiveWitness[JObject](3)
  implicit val MongoPrimitiveJArrayWitness  = new MongoPrimitiveWitness[JArray](4)
  implicit val MongoPrimitiveJBoolWitness   = new MongoPrimitiveWitness[JBool](8)
  implicit val MongoPrimitiveJNullWitness   = new MongoPrimitiveWitness[JNull.type](10)
  implicit val MongoPrimitiveJIntWitness    = new MongoPrimitiveWitness[JInt](18)
}
object MongoFilterImplicits extends MongoFilterImplicits

/** The MongoFilterBuilder creates mongo filters. Filters can be used in MongoQuery in where clause.
 * <p>
 * <pre>
 * import blueeyes.persistence.mongo.MongoImplicits._
 * import blueeyes.persistence.mongo.MongoQueryBuilder._
 *
 * val filter = "foo.bar" === "blahbMongoPrimitiveJObjectlah"
 *
 * val query  = selectOne().from("mycollection").where(filter)
 * val query2 = selectOne().from("mycollection").where("foo.bar" === "blahblah")
 * </pre>
 * <p>
 * Filters can be combined.
 * <p>
 * <pre>
 * val filter  = ("foo.bar" === "blahblah") | ("foo.bar" === "blah")
 * val filter2 = ("foo" === "blahblah") & ("bar" === "blah")
 * </pre>
 * <p>
 */
case class MongoFilterBuilder(jpath: JPath) {
  import MongoFilterImplicits._
  def === [T](value: MongoPrimitive): MongoFilter = MongoFieldFilter(jpath, $eq, value)

  def !== [T](value: MongoPrimitive): MongoFilter = MongoFieldFilter(jpath, $ne, value)

  def > [T](value: MongoPrimitive): MongoFilter = MongoFieldFilter(jpath, $gt, value)

  def >= [T](value: MongoPrimitive): MongoFilter = MongoFieldFilter(jpath, $gte, value)

  def < [T](value: MongoPrimitive): MongoFilter = MongoFieldFilter(jpath, $lt, value)

  def <= [T](value: MongoPrimitive): MongoFilter = MongoFieldFilter(jpath, $lte, value)

  def anyOf[T <: MongoPrimitive](items: T*): MongoFilter = MongoFieldFilter(jpath, $in, List(items: _*))

  def contains[T <: MongoPrimitive](items: T*): MongoFilter = MongoFieldFilter(jpath, $all, List(items: _*))

  def hasSize(length: Int): MongoFilter =  MongoFieldFilter(jpath, $size, length)

  def isDefined: MongoFilter = MongoFieldFilter(jpath, $exists, true)

  def hasType[T](implicit witness: MongoPrimitiveWitness[T]): MongoFilter = MongoFieldFilter(jpath, $type, witness.typeNumber)
}

private[mongo] object JPathExtension{
  def toMongoField(path: JPath) = {
    val mongoPath = JPath(path.nodes.map{ node =>
      node match{
        case e: JPathIndex => JPathField(e.index.toString)
        case _ => node
      }
    })
    if (mongoPath.path.startsWith(".")) mongoPath.path.substring(1) else mongoPath.path
  }
}
