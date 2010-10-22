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
  
  def & (that: MongoQuery): MongoQuery = MongoAndQuery(self, that)
  
  def && (that: MongoQuery): MongoQuery = MongoAndQuery(self, that)

  def | (that: MongoQuery): MongoQuery = MongoOrQuery(self, that)
  
  def || (that: MongoQuery): MongoQuery = MongoOrQuery(self, that)
  
  def combine (that: MongoQuery): MongoQuery
  
  def * (that: MongoQuery): MongoQuery = combine(that)
  
  def commutesWith(that: MongoQuery): Boolean
  
  def combinesWith(that: MongoQuery): Boolean
  
  def unary_! : MongoQuery
}

sealed trait MongoFieldQuery extends MongoQuery { self =>
  def lhs: JPath
  
  def commutesWith(that: MongoQuery): Boolean = that match {
    case that: MongoFieldQuery => self.lhs != that.lhs
    
    case _ => false
  }
  
  def combinesWith(that: MongoQuery): Boolean = that match {
    case that: MongoFieldQuery => self.lhs == that.lhs
    
    case _ => false
  }
}
sealed case class MongoFieldQueryOp1(lhs: JPath, operator: MongoQueryOperator, rhs: MongoPrimitive[_]) extends MongoFieldQuery {
  override def query: JField = JField(lhs.path, JObject(JField(operator.symbol, rhs.toJValue) :: Nil))
  
  def combine (that: MongoQuery): MongoQuery = error("not implemented")
  
  def unary_! : MongoQuery = MongoFieldQueryOp1(lhs, !operator, rhs)
}
sealed case class MongoFieldQueryOp2(lhs: JPath, operator1: MongoQueryOperator, rhs1: MongoPrimitive[_], operator2: MongoQueryOperator, rhs2: MongoPrimitive[_]) extends MongoFieldQuery {
  override def query: JField = JField(lhs.path, JObject(JField(operator1.symbol, rhs1.toJValue) :: JField(operator2.symbol, rhs2.toJValue) :: Nil))
  
  def combine (that: MongoQuery): MongoQuery = error("not implemented")
  
  def unary_! : MongoQuery = MongoFieldQueryOp2(lhs, !operator1, rhs1, !operator2, rhs2)
}

sealed case class MongoOrQuery private (alternatives: List[MongoQuery]) extends MongoQuery {
  override def query: JObject = JObject(JField($or.symbol, JArray(alternatives.map(_.query))) :: Nil)
  
  def combine (that: MongoQuery): MongoQuery = error("not implemented")
  
  def unary_! : MongoQuery = error("not implemented")
  
  def commutesWith(that: MongoQuery): Boolean = true
  
  def combinesWith(that: MongoQuery): Boolean = false
}
object MongoOrQuery {
  def apply(self: MongoQuery, that: MongoQuery): MongoOrQuery = MongoOrQuery(self :: that :: Nil)
}

sealed case class MongoAndQuery private (queries: List[MongoQuery]) extends MongoQuery {
  def query: JValue = queries.foldLeft(JObject(Nil): JValue) { (obj, e) => obj.merge(e.query) }
  
  def combine (that: MongoQuery): MongoQuery = error("not implemented")
  
  def unary_! : MongoQuery = error("not implemented")
  
  def commutesWith(that: MongoQuery): Boolean = true
  
  def combinesWith(that: MongoQuery): Boolean = false
}
object MongoAndQuery {
  def apply(self: MongoQuery, that: MongoQuery): MongoAndQuery = new MongoAndQuery(flatten(that).foldLeft(flatten(self)) { (list, e) => merge(e, list, Nil) })
  
  private def merge(q: MongoQuery, before: List[MongoQuery], after: List[MongoQuery]): List[MongoQuery] = after match {
    case Nil => before ::: q :: Nil
    
    case x :: xs => q.combinesWith(x) match {
      case true  => before ::: q.combine(x) :: xs
      
      case false => q.commutesWith(x) match {
        case true  => before ::: (x :: Nil) ::: merge(q, before, xs)
        
        case false => before ::: q :: after
      }
    }
  }
  
  private def flatten(q: MongoQuery): List[MongoQuery] = q match {
    case x: MongoAndQuery => x.queries
    
    case x => x :: Nil
  }
}


sealed trait MongoPrimitive[T] {
  def toJValue: JValue
}

sealed class MongoPrimitiveWitness[T]

case class MongoQueryBuilder(jpath: JPath) {
  def === [T](value: MongoPrimitive[T]): MongoFieldQuery = error("not implemented")
  
  def !== [T](value: MongoPrimitive[T]): MongoFieldQuery = error("not implemented")
  
  def > (value: Long): MongoFieldQuery = error("not implemented")
  
  def >= (value: Long): MongoFieldQuery = error("not implemented")
  
  def < (value: Long): MongoFieldQuery = error("not implemented")
  
  def <= (value: Long): MongoFieldQuery = error("not implemented")
  
  def in [T <: MongoPrimitive[T]](items: T*): MongoFieldQuery = error("not implemented")
  
  def contains [T <: MongoPrimitive[T]](items: T*): MongoFieldQuery = error("not implemented")
  
  def hasSize(length: Int): MongoFieldQuery = error("not implemented")
  
  def exists: MongoFieldQuery = error("not implemented")
  
  def hasType[T](implicit witness: MongoPrimitiveWitness[T]): MongoFieldQuery = error("not implemented")
}


trait MongoQueryImplicits {
  import MongoQueryOperators._ 
  
  implicit def mongoOperatorToSymbolString(op: MongoQueryOperator): String = op.symbol
  
  implicit def stringToMongoQueryBuilder(string: String): MongoQueryBuilder = MongoQueryBuilder(JPath(string))
  
  implicit def jpathToMongoQueryBuilder(jpath: JPath): MongoQueryBuilder = MongoQueryBuilder(jpath)
  
  case class MongoPrimitiveString(value: String) extends MongoPrimitive[String] {
    def toJValue = JString(value)
  }
  
  implicit def stringToMongoPrimitiveString(value: String) = MongoPrimitiveString(value)
  
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