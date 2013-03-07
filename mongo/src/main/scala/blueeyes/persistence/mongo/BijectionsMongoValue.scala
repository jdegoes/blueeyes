package blueeyes.persistence.mongo

import java.util.Date
import org.joda.time.DateTime

import com.mongodb.{BasicDBObject, DBObject}
import org.bson.types.ObjectId

import scala.collection.JavaConverters._
import scalaz.{Success, NonEmptyList, ValidationNel}
import scalaz.Validation._
import scalaz.Scalaz._

class MongoObjectBijection extends MongoBijection[MongoValue, MongoField, MongoObject]{
  def extractString (value: String)                  = MongoString(value).success
  def extractInteger(value: java.lang.Integer)       = MongoInt(value).success
  def extractLong   (value: java.lang.Long)          = MongoLong(value).success
  def extractFloat  (value: java.lang.Float)         = MongoFloat(value).success
  def extractDouble (value: java.lang.Double)        = MongoDouble(value).success
  def extractBoolean(value: java.lang.Boolean)       = MongoBoolean(value).success
  def extractDate   (value: Date)                    = MongoDate(new DateTime(value)).success
  def extractRegex  (value: java.util.regex.Pattern) = MongoRegex(value.pattern, if (value.flags != 0) Some(value.flags) else None).success
  def extractBinary (value: Array[Byte])             = MongoBinary(value).success
  def extractId     (value: ObjectId)                = MongoId(value.toByteArray).success
  def buildNull                                      = MongoNull.success

  def buildArray(value: Seq[MongoValue])             = MongoArray(value.toList)
  def buildField(name: String, value: MongoValue)    = MongoField(name, value)
  def buildObject(fields: Seq[MongoField])           = MongoObject(fields.toList)

  def unapply(mobject: MongoObject): ValidationNel[String, DBObject] = {
    mobject.fields.foldLeft(success[NonEmptyList[String], BasicDBObject](new BasicDBObject)) {
      case (Success(obj), MongoField(name, value)) => toStorageValue(value).map(obj.append(name, _))
      case (failure, _) => failure
    }
  }

  private def toStorageValue(mvalue: MongoValue): ValidationNel[String, Any] = mvalue match {
    case MongoArray(values)   => values.map(toStorageValue).sequence[({type N[B] = ValidationNel[String, B]})#N, Any].map(_.asJava)
    case MongoInt(value)      => success(new java.lang.Integer(value))
    case MongoLong(value)     => success(new java.lang.Long(value))
    case MongoFloat(value)    => success(new java.lang.Float(value))
    case MongoDouble(value)   => success(new java.lang.Double(value))
    case MongoBoolean(value)  => success(new java.lang.Boolean(value))
    case MongoDate(value)     => success(value.toDate)
    case MongoRegex(pattern, flags) => success(flags.map(java.util.regex.Pattern.compile(pattern, _)).getOrElse(java.util.regex.Pattern.compile(pattern)))
    case MongoId(bytes)       => success(new ObjectId(bytes))
    case obj: MongoObject     => unapply(obj)
    case other                => success(other.values)
  }
}

trait BijectionsMongoValue {
  implicit val MongoToMongoValue = new MongoObjectBijection
  implicit val MongoValueToMongo = MongoToMongoValue.inverse
}

object BijectionsMongoValue extends BijectionsMongoValue
