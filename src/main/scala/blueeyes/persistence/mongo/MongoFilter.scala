package blueeyes.persistence.mongo

import blueeyes.json._
import blueeyes.json.JsonAST._

import blueeyes.util.ProductPrefixUnmangler

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
  
  case object $mod        extends MongoFilterOperator { def unary_! : MongoFilterOperator = error("The $mod operator does not have a negation"); }
  case object $all        extends MongoFilterOperator { def unary_! : MongoFilterOperator  = error("The $all operator does not have a negation"); }
  case object $size       extends MongoFilterOperator { def unary_! : MongoFilterOperator  = error("The $size operator does not have a negation"); }
  case object $exists     extends MongoFilterOperator { def unary_! : MongoFilterOperator  = error("The $exists operator does not have a negation"); }
  case object $type       extends MongoFilterOperator { def unary_! : MongoFilterOperator  = error("The $type operator does not have a negation"); }
  case object $or         extends MongoFilterOperator { def unary_! : MongoFilterOperator  = error("The $or operator does not have a negation"); }
  case object $each       extends MongoFilterOperator { def unary_! : MongoFilterOperator  = error("The $each operator does not have a negation"); }
}

import MongoFilterOperators._

sealed trait MongoFilter { self =>
  def filter: JValue
  
  def unary_! : MongoFilter
  
  def & (that: MongoFilter)  = &&(that)
  
  def && (that: MongoFilter) = MongoAndFilter(self :: that :: Nil)

  def | (that: MongoFilter)  = ||(that)
  
  def || (that: MongoFilter) = (self, that) match{
    case (x: MongoOrFilter, y: MongoOrFilter)    => MongoOrFilter(x.queries ++ y.queries)
    case (_, y: MongoOrFilter) => MongoOrFilter(self :: y.queries)
    case (x: MongoOrFilter, _) => MongoOrFilter(x.queries :+ that)
    case (MongoFilterAll, _)   => that
    case (_, MongoFilterAll)   => self
    case _  => MongoOrFilter(self :: that :: Nil)
  }
}

case object MongoFilterAll extends MongoFilter {
  def filter: JValue = JObject(Nil)
  
  def unary_! : MongoFilter = this
}

sealed case class MongoFieldFilter(lhs: JPath, operator: MongoFilterOperator, rhs: MongoPrimitive[_]) extends MongoFilter { self =>
  def filter: JValue = {
    val value = operator match {
      case $eq => rhs.toJValue

      case _ => JObject(JField(operator.symbol, rhs.toJValue) :: Nil)
    }
    lhs.nodes match{
      case Nil => value
      case _   => JObject(JField(JPathExtension.toMongoField(lhs), value) :: Nil)
    }
  }

  def unary_! : MongoFilter = MongoFieldFilter(lhs, !operator, rhs)
}

sealed case class MongoOrFilter(queries: List[MongoFilter]) extends MongoFilter {
  def filter: JValue = JObject(JField($or.symbol, JArray(queries.map(_.filter))) :: Nil)
  
  def unary_! : MongoFilter = MongoAndFilter(queries.map(!_))
}

sealed case class MongoAndFilter(queries: List[MongoFilter]) extends MongoFilter { self =>
  def filter: JValue = {
    queries.foldLeft(JObject(Nil).asInstanceOf[JValue]) { (obj, e) => obj.merge(e.filter) }
  }
  
  def unary_! : MongoFilter = MongoOrFilter(queries.map(!_))

  def elemMatch(path: JPath) = MongoElementsMatchFilter(path, self)
}

sealed case class MongoElementsMatchFilter(lhs: JPath, elementsQuery: MongoAndFilter) extends MongoFilter{
  def unary_! = error("The $elemMatch operator does not have a negation")

  def filter = {
    val value = JObject(JField("$elemMatch", elementsQuery.filter) :: Nil)
    lhs.nodes match{
      case Nil => value
      case _   => JObject(JField(JPathExtension.toMongoField(lhs), value) :: Nil)
    }
  }
}

sealed trait MongoPrimitive[T] {
  def toJValue: JValue
}

sealed class MongoPrimitiveWitness[T](val typeNumber: Int)

trait MongoFilterImplicits {
  import MongoFilterOperators._ 
  
  implicit def mongoOperatorToSymbolString(op: MongoFilterOperator): String = op.symbol
  
  implicit def stringToMongoFilterBuilder(string: String): MongoFilterBuilder = MongoFilterBuilder(JPath(string))
  
  implicit def jpathToMongoFilterBuilder(jpath: JPath): MongoFilterBuilder = MongoFilterBuilder(jpath)
  
  case class MongoPrimitiveString(value: String) extends MongoPrimitive[String] {
    def toJValue = JString(value)
  }
  case class MongoPrimitiveLong(value: Long) extends MongoPrimitive[Long] {
    def toJValue = JInt(value)
  }
  case class MongoPrimitiveInt(value: Int) extends MongoPrimitive[Int] {
    def toJValue = JInt(value)
  }
  case class MongoPrimitiveDouble(value: Double) extends MongoPrimitive[Double] {
    def toJValue = JDouble(value)
  }
  case class MongoPrimitiveBoolean(value: Boolean) extends MongoPrimitive[Boolean] {
    def toJValue = JBool(value)
  }
  case class MongoPrimitiveArray[T <: MongoPrimitive[_]](value: List[T]) extends MongoPrimitive[List[T]] {
    def toJValue = JArray(value.map(_.toJValue))
  }
  case class MongoPrimitiveJObject(value: JObject) extends MongoPrimitive[JObject] {
    def toJValue = value
  }
  case object MongoPrimitiveNull extends MongoPrimitive[Any] {
    def toJValue = JNull
  }

  implicit def jvalueToMongoPrimitive(value: JValue): Option[MongoPrimitive[_]] = {
    val mongoPromitive: Option[MongoPrimitive[_]] = value match {
      case x: JString => Some(MongoPrimitiveString(x.value))
      case x: JInt    => Some(MongoPrimitiveLong(x.value.longValue))
      case x: JDouble => Some(MongoPrimitiveDouble(x.value))
      case x: JBool   => Some(MongoPrimitiveBoolean(x.value))
      case x: JObject => Some(MongoPrimitiveJObject(x))
      case x: JArray  => {
        var values: List[MongoPrimitive[_]] = x.elements.map(jvalueToMongoPrimitive(_)).filter(_ != None).map(_.get)
        Some(MongoPrimitiveArray(values))
      }
      case JNothing   => None
      case JNull      => None
      case _          => error("Unknown type for value: " + value)
    }
    mongoPromitive
  }

  implicit def stringToMongoPrimitiveString(value: String)    = MongoPrimitiveString(value)
  implicit def longToMongoPrimitiveLong(value: Long)          = MongoPrimitiveLong(value)
  implicit def intToMongoPrimitiveInt(value: Int)             = MongoPrimitiveInt(value)
  implicit def doubleToMongoPrimitiveDouble(value: Double)    = MongoPrimitiveDouble(value)
  implicit def booleanToMongoPrimitiveBoolean(value: Boolean) = MongoPrimitiveBoolean(value)
  implicit def jobjectToMongoPrimitiveJObject(value: JObject) = MongoPrimitiveJObject(value)
  implicit def arryaToMongoPrimitiveArray[T <: MongoPrimitive[_]](value: List[T]) = MongoPrimitiveArray[T](value)

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
 * val filter = "foo.bar" === "blahblah"
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
  def === [T](value: MongoPrimitive[T]): MongoFieldFilter = MongoFieldFilter(jpath, $eq, value)

  def !== [T](value: MongoPrimitive[T]): MongoFieldFilter = MongoFieldFilter(jpath, $ne, value)

  def > [T](value: MongoPrimitive[T]): MongoFieldFilter = MongoFieldFilter(jpath, $gt, value)

  def >= [T](value: MongoPrimitive[T]): MongoFieldFilter = MongoFieldFilter(jpath, $gte, value)

  def < [T](value: MongoPrimitive[T]): MongoFieldFilter = MongoFieldFilter(jpath, $lt, value)

  def <= [T](value: MongoPrimitive[T]): MongoFieldFilter = MongoFieldFilter(jpath, $lt, value)

  def in [T <: MongoPrimitive[_]](items: T*): MongoFieldFilter = MongoFieldFilter(jpath, $in, List(items: _*))

  def contains [T <: MongoPrimitive[_]](items: T*): MongoFieldFilter = MongoFieldFilter(jpath, $all, List(items: _*))

  def hasSize(length: Int): MongoFieldFilter =  MongoFieldFilter(jpath, $size, length)

  def exists: MongoFieldFilter = MongoFieldFilter(jpath, $exists, true)

  def hasType[T](implicit witness: MongoPrimitiveWitness[T]): MongoFieldFilter = MongoFieldFilter(jpath, $type, witness.typeNumber)
}

private[mongo] object JPathExtension{
  def toMongoField(path: JPath) = if (path.path.startsWith(".")) path.path.substring(1) else path.path

}
