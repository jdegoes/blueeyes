package blueeyes.persistence.mongo.json

import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo.MongoBijection

import com.mongodb.{BasicDBObject, DBObject}
import org.bson.types.ObjectId
import org.apache.commons.codec.binary.Hex

import scala.collection.JavaConverters._
import scalaz.{Validation, Success, NonEmptyList}
import scalaz.Scalaz._

class MongoJValueBijection extends MongoBijection[JValue, JField, JObject]{
  def extractString (value: String)                  = JString(value).success
  def extractInteger(value: java.lang.Integer)       = JInt(value.intValue).success
  def extractLong   (value: java.lang.Long)          = JInt(value.longValue).success
  def extractFloat  (value: java.lang.Float)         = JDouble(value.doubleValue).success
  def extractDouble (value: java.lang.Double)        = JDouble(value.doubleValue).success
  def extractBoolean(value: java.lang.Boolean)       = JBool(value).success
  def extractDate   (value: java.util.Date)          = "Date type is not supported".fail.liftFailNel
  def extractRegex  (value: java.util.regex.Pattern) = "Regex type is not supported".fail.liftFailNel
  def extractBinary (value: Array[Byte])             = "Binary type is not supported".fail.liftFailNel
  def extractId     (value: ObjectId)                = JString("ObjectId(\"" + Hex.encodeHexString(value.toByteArray) + "\")").success
  def buildNull                                      = JNull.success

  def buildArray(value: Seq[JValue])                 = JArray(value.toList)
  def buildField(name: String, value: JValue)        = JField(name, value)
  def buildObject(fields: Seq[JField])               = JObject(fields.toList)

  def unapply(jobject: JObject): ValidationNEL[String, DBObject] = {
    jobject.fields.foldLeft(success[NonEmptyList[String], BasicDBObject](new BasicDBObject)) {
      case (success @ Success(obj), JField(name, JNothing)) => success
      case (Success(obj), JField(name, value)) => toStorageValue(value).map(obj.append(name, _))
      case (failure, _) => failure
    }
  }

  private val ObjectIdPattern = """ObjectId\("([0-9a-f]*)"\)""".r

  private def toStorageValue(v: JValue): ValidationNEL[String, Any] = v match {
    case JNull | JNothing => success(null)
    case JString(ObjectIdPattern(id)) => new ObjectId(Hex.decodeHex(id.toCharArray)).success
    case JString(x) => success(x)
    case JInt(x)    => success(x.longValue)
    case JDouble(x) => success(x.doubleValue)
    case JBool(x)   => success(x)
    case JArray(x)  => x.map(toStorageValue).sequence[({type N[B] = ValidationNEL[String, B]})#N, Any].map(_.asJava)
    case jobject: JObject => unapply(jobject)
  }
}

trait BijectionsMongoJson {
  implicit val MongoToJson = new MongoJValueBijection
  implicit val JsonToMongo = MongoToJson.inverse
}

object BijectionsMongoJson extends BijectionsMongoJson

