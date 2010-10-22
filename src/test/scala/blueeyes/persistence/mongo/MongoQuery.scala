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

trait MongoQuery { self =>
  def elements: List[MongoSimpleQuery]
  
  def query: JValue = elements.foldLeft(JObject(Nil)) { (obj, e) => (obj ++ e.query).asInstanceOf[JObject] }
  
  def & (that: MongoQuery): MongoQuery =  this && that
  
  def && (that: MongoQuery): MongoQuery = {
    def merge(q: MongoSimpleQuery, before: List[MongoSimpleQuery], after: List[MongoSimpleQuery]): List[MongoSimpleQuery] = after match {
      case Nil => before ::: q :: Nil
      
      case x :: xs => q.combinesWith(x) match {
        case true  => before ::: q.combine(x) :: xs
        
        case false => q.commutesWith(x) match {
          case true  => before ::: (x :: Nil) ::: merge(q, before, xs)
          
          case false => before ::: q :: after
        }
      }
    }
    
    new MongoQuery {
      val elements = that.elements.foldLeft(self.elements) { (list, e) => merge(e, list, Nil) }
    }
  }

  def | (that: MongoQuery): MongoQuery = this || that
  
  def || (that: MongoQuery): MongoQuery = error("not implemented")
}

sealed case class MongoSimpleQuery(lhs: JPath, operator: MongoQueryOperator, rhs: MongoPrimitive[_]) extends MongoQuery { self =>
  def elements = self :: Nil
  
  override def query: JField = JField(lhs.path, JObject(JField(operator.symbol, rhs.toJValue) :: Nil))
  
  def commutesWith(that: MongoSimpleQuery): Boolean = self.lhs != that.lhs
  
  def combinesWith(that: MongoSimpleQuery): Boolean = (self.lhs == that.lhs)
  
  def combine (that: MongoSimpleQuery): MongoSimpleQuery = error("not implemented")
  
  def * (that: MongoSimpleQuery): MongoSimpleQuery = combine(that)
  
  def unary_! : MongoQuery = MongoSimpleQuery(lhs, !operator, rhs)
}


sealed trait MongoPrimitive[T] {
  def toJValue: JValue
}

sealed class MongoPrimitiveWitness[T]

case class MongoQueryBuilder(jpath: JPath) {
  def === [T](value: MongoPrimitive[T]): MongoSimpleQuery = error("not implemented")
  
  def !== [T](value: MongoPrimitive[T]): MongoSimpleQuery = error("not implemented")
  
  def > (value: Long): MongoSimpleQuery = error("not implemented")
  
  def >= (value: Long): MongoSimpleQuery = error("not implemented")
  
  def < (value: Long): MongoSimpleQuery = error("not implemented")
  
  def <= (value: Long): MongoSimpleQuery = error("not implemented")
  
  def in [T <: MongoPrimitive[T]](items: T*): MongoSimpleQuery = error("not implemented")
  
  def contains [T <: MongoPrimitive[T]](items: T*): MongoSimpleQuery = error("not implemented")
  
  def hasSize(length: Int): MongoSimpleQuery = error("not implemented")
  
  def exists: MongoSimpleQuery = error("not implemented")
  
  def hasType[T](implicit witness: MongoPrimitiveWitness[T]): MongoSimpleQuery = error("not implemented")
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