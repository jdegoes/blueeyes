package blueeyes.persistence.mongo

import blueeyes.json._
import blueeyes.json.JsonAST._

import blueeyes.util.ProductPrefixUnmangler

sealed abstract class MongoSortOrder(val order: Int)
case object MongoSortOrderAscending extends MongoSortOrder(1)
case object MongoSortOrderDescending extends MongoSortOrder(-1)

object MongoQueryOperators {
  sealed trait MongoQueryOperator extends Product with ProductPrefixUnmangler {
    def symbol: String = unmangledName
  
    def unary_! : MongoQueryOperator
    
    override def toString = symbol
  }
  sealed trait MongoQueryOperatorBound extends MongoQueryOperator
  case object $gt   extends MongoQueryOperatorBound  { def unary_! = $lte; }
  case object $gte  extends MongoQueryOperatorBound  { def unary_! = $lt; }
  case object $lt   extends MongoQueryOperatorBound  { def unary_! = $gte; }
  case object $lte  extends MongoQueryOperatorBound  { def unary_! = $gt; }
  
  sealed trait MongoQueryOperatorEquality extends MongoQueryOperator
  case object $eq extends MongoQueryOperatorEquality { def unary_! = $ne; } // This is a virtual operator, it's not real!!!!
  case object $ne extends MongoQueryOperatorEquality { def unary_! = $eq; }
  
  sealed trait MongoQueryOperatorContainment extends MongoQueryOperator
  case object $in   extends MongoQueryOperatorContainment { def unary_! = $nin; }
  case object $nin  extends MongoQueryOperatorContainment { def unary_! = $in; }
  
  case object $mod    extends MongoQueryOperator { def unary_! = error("The $mod operator does not have a negation"); }
  case object $all    extends MongoQueryOperator { def unary_! = error("The $all operator does not have a negation"); }
  case object $size   extends MongoQueryOperator { def unary_! = error("The $size operator does not have a negation"); }
  case object $exists extends MongoQueryOperator { def unary_! = error("The $exists operator does not have a negation"); }
  case object $type   extends MongoQueryOperator { def unary_! = error("The $type operator does not have a negation"); }
  case object $or     extends MongoQueryOperator { def unary_! = error("The $or operator does not have a negation"); }
}

import MongoQueryOperators._

sealed trait MongoQuery { self =>
  def query: JValue
  
  def unary_! : MongoQuery
  
  def & (that: MongoQuery): MongoQuery = MongoAndQuery(self :: that :: Nil)
  
  def && (that: MongoQuery): MongoQuery = MongoAndQuery(self :: that :: Nil)

  def | (that: MongoQuery): MongoQuery = MongoOrQuery(self :: that :: Nil)
  
  def || (that: MongoQuery): MongoQuery = MongoOrQuery(self :: that :: Nil)
}

sealed case class MongoFieldQuery(lhs: JPath, operator: MongoQueryOperator, rhs: MongoPrimitive[_]) extends MongoQuery { self =>
  def query: JField = operator match {
    case $eq => JField(lhs.path, rhs.toJValue)
    
    case _ => JField(lhs.path, JObject(JField(operator.symbol, rhs.toJValue) :: Nil))
  }
    
  def unary_! : MongoQuery = MongoFieldQuery(lhs, !operator, rhs)
}

sealed case class MongoOrQuery(queries: List[MongoQuery]) extends MongoQuery {
  def query: JObject = JObject(JField($or.symbol, JArray(queries.map(_.query))) :: Nil)
  
  def unary_! : MongoQuery = MongoAndQuery(queries.map(!_))
}

sealed case class MongoAndQuery(queries: List[MongoQuery]) extends MongoQuery {
  def query: JValue = queries.foldLeft(JObject(Nil): JValue) { (obj, e) => obj.merge(e.query) }
  
  def unary_! : MongoQuery = MongoOrQuery(queries.map(!_))
}


sealed trait MongoPrimitive[T] {
  def toJValue: JValue
}

sealed class MongoPrimitiveWitness[T]

trait MongoQueryImplicits {
  import MongoQueryOperators._ 
  
  implicit def mongoOperatorToSymbolString(op: MongoQueryOperator): String = op.symbol
  
  implicit def stringToMongoQueryBuilder(string: String): MongoQueryBuilder = MongoQueryBuilder(JPath(string))
  
  implicit def jpathToMongoQueryBuilder(jpath: JPath): MongoQueryBuilder = MongoQueryBuilder(jpath)
  
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
//  case class MongoPrimitiveArray[T](value: List[T]) extends MongoPrimitive[List[T]] {
//    def toJValue = JArray(value)
//  }

  implicit def stringToMongoPrimitiveString(value: String)    = MongoPrimitiveString(value)
  implicit def longToMongoPrimitiveLong(value: Long)          = MongoPrimitiveLong(value)
  implicit def intToMongoPrimitiveInt(value: Int)             = MongoPrimitiveInt(value)
  implicit def doubleToMongoPrimitiveDouble(value: Double)    = MongoPrimitiveDouble(value)
  implicit def booleanToMongoPrimitiveBoolean(value: Boolean) = MongoPrimitiveBoolean(value)

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
  implicit val MongoPrimitiveJStringWitness = new MongoPrimitiveWitness[JString]
  implicit val MongoPrimitiveJDoubleWitness = new MongoPrimitiveWitness[JDouble]
  implicit val MongoPrimitiveJObjectWitness = new MongoPrimitiveWitness[JObject]
  implicit val MongoPrimitiveJArrayWitness = new MongoPrimitiveWitness[JArray]
  implicit val MongoPrimitiveJBoolWitness = new MongoPrimitiveWitness[JBool]
  implicit val MongoPrimitiveJNullWitness = new MongoPrimitiveWitness[JNull.type]
  implicit val MongoPrimitiveJIntWitness = new MongoPrimitiveWitness[JInt]
  

    // case class MongoPrimitiveString    - def toMongoValue: String
    // case class MongoPrimitiveLong      - def toMongoValue: java.lang.Long
    // case class MongoPrimitiveInteger   - def toMongoValue: java.lang.Integer
    // case class MongoPrimitiveDouble    - def toMongoValue: java.lang.Double
    // case class MongoPrimitiveFloat     - def toMongoValue: java.lang.Float
    // case class MongoPrimitiveBoolean   - def toMongoValue: java.lang.Boolean
    // case class MongoPrimitiveArrayList - def toMongoValue: java.util.ArrayList[AnyRef]
    // case class MongoPrimitiveDBObject  - def toMongoValue: DBObject
    // case class MongoPrimitiveNull      - def toMongoValue: null
    // case class MongoPrimitiveObjectId  - def toMongoValue: com.mongodb.ObjectId
    // case class MongoPrimitivePattern   - def toMongoValue: java.util.regex.Pattern
    // case class MongoPrimitiveDate      - def toMongoValue: java.util.Date
    // case class MongoPrimitiveDBRef     - def toMongoValue: com.mongodb.DBRef
    // case class MongoPrimitiveByte      - def toMongoValue: byte[]
}
object MongoQueryImplicits extends MongoQueryImplicits

case class MongoQueryBuilder(jpath: JPath) {
  import MongoQueryImplicits._
  def === [T](value: MongoPrimitive[T]): MongoFieldQuery = MongoFieldQuery(jpath, $eq, value)

  def !== [T](value: MongoPrimitive[T]): MongoFieldQuery = MongoFieldQuery(jpath, $ne, value)

  def > [T](value: MongoPrimitive[T]): MongoFieldQuery = MongoFieldQuery(jpath, $gt, value)

  def >= [T](value: MongoPrimitive[T]): MongoFieldQuery = MongoFieldQuery(jpath, $gte, value)

  def < [T](value: MongoPrimitive[T]): MongoFieldQuery = MongoFieldQuery(jpath, $lt, value)

  def <= [T](value: MongoPrimitive[T]): MongoFieldQuery = MongoFieldQuery(jpath, $lt, value)

  def in [T <: MongoPrimitive[T]](items: T*): MongoFieldQuery = error("not implemented")//MongoFieldQuery(jpath, $in, MongoPrimitiveArray(items.map(_.toJValue)))

  def contains [T <: MongoPrimitive[T]](items: T*): MongoFieldQuery = error("not implemented")//MongoFieldQuery(jpath, $all, MongoPrimitiveArray(items.map(_.toJValue)))

  def hasSize(length: Int): MongoFieldQuery =  MongoFieldQuery(jpath, $size, length)

  def exists: MongoFieldQuery = MongoFieldQuery(jpath, $exists, true)

  def hasType[T](implicit witness: MongoPrimitiveWitness[T]): MongoFieldQuery = error("not implemented")
}
