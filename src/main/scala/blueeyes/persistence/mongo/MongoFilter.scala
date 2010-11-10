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
  
  def & (that: MongoFilter)  = MongoAndFilter(self :: that :: Nil)
  
  def && (that: MongoFilter) = MongoAndFilter(self :: that :: Nil)

  def | (that: MongoFilter)  = MongoOrFilter(self :: that :: Nil)
  
  def || (that: MongoFilter) = MongoOrFilter(self :: that :: Nil)
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
  

    // case class MongoPrimitiveString    - def toMongoValue: String
    // case class MongoPrimitiveLong      - def toMongoValue: java.lang.Long
    // case class MongoPrimitiveInteger   - def toMongoValue: java.lang.Integer
    // case class MongoPrimitiveDouble    - def toMongoValue: java.lang.Double
    // case class MongoPrimitiveFloat     - def toMongoValue: java.lang.Float
    // case class MongoPrimitiveBoolean   - def toMongoValue: java.lang.Boolean
    // case class MongoPrimitiveArrayList - def toMongoValue: java.util.ArrayList[AnyRef]
    // case class MongoPrimitiveDBObject  - def toMongoValue: DBObject
    // case class MongoPrimitiveNull      -   def toMongoValue: null
    // case class MongoPrimitiveObjectId  - def toMongoValue: com.mongodb.ObjectId
    // case class MongoPrimitivePattern   - def toMongoValue: java.util.regex.Pattern
    // case class MongoPrimitiveDate      - def toMongoValue: java.util.Date
    // case class MongoPrimitiveDBRef     - def toMongoValue: com.mongodb.DBRef
    // case class MongoPrimitiveByte      - def toMongoValue: byte[]
}
object MongoFilterImplicits extends MongoFilterImplicits

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
