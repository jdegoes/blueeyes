package blueeyes.persistence.mongo

import blueeyes.json._
import blueeyes.json.JsonAST._

trait MongoQuery {
  def query: JValue
  
  def & (that: MongoQuery): MongoQuery = error("not implemented")
  
  def && (that: MongoQuery): MongoQuery = error("not implemented")
  
  def || (that: MongoQuery): MongoQuery = error("not implemented")
  
  def | (that: MongoQuery): MongoQuery = error("not implemented")
  
  def unary_! : MongoQuery = error("not implemented")
}

trait MongoSimpleQuery extends MongoQuery { self =>
  def elements = self :: Nil
  
  def jpath: JPath
  
  def combinesWith(that: MongoSimpleQuery): Boolean = (self.jpath == that.jpath)
  
  def * (that: MongoSimpleQuery): MongoSimpleQuery
}


sealed trait MongoPrimitive[T] {
  def toMongoValue: Any
}

case class MongoQueryBuilder(jpath: JPath) {
  def === [T](value: MongoPrimitive[T]): MongoQuery = error("not implemented")
  
  def !== [T](value: MongoPrimitive[T]): MongoQuery = error("not implemented")
  
  def > (value: Long): MongoQuery = error("not implemented")
  
  def >= (value: Long): MongoQuery = error("not implemented")
  
  def < (value: Long): MongoQuery = error("not implemented")
  
  def <= (value: Long): MongoQuery = error("not implemented")
}


trait MongoQueryImplicits {
  implicit def stringToMongoQueryBuilder(string: String): MongoQueryBuilder = MongoQueryBuilder(JPath(string))
  
  implicit def jpathToMongoQueryBuilder(jpath: JPath): MongoQueryBuilder = MongoQueryBuilder(jpath)
  
  case class MongoPrimitiveString(value: String) extends MongoPrimitive[String] {
    def toMongoValue = new java.lang.String(value)
  }
  
  implicit def stringToMongoPrimitiveString(value: String) = MongoPrimitiveString(value)

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