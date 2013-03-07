package blueeyes.persistence.mongo

import blueeyes.core.data.PartialBijection
import com.mongodb.{BasicDBObject, DBObject}
import java.util.regex.Pattern
import org.bson.types.ObjectId

import scala.collection.JavaConverters._
import scalaz.Validation
import scalaz.ValidationNel
import scalaz.Scalaz._


trait MongoBijection[V, F, O <: V] extends PartialBijection[DBObject, O] {
  def extractString (value: String): ValidationNel[String, V]
  def extractInteger(value: java.lang.Integer): ValidationNel[String, V]
  def extractLong   (value: java.lang.Long): ValidationNel[String, V]
  def extractFloat  (value: java.lang.Float): ValidationNel[String, V]
  def extractDouble (value: java.lang.Double): ValidationNel[String, V]
  def extractBoolean(value: java.lang.Boolean): ValidationNel[String, V]
  def extractDate   (value: java.util.Date): ValidationNel[String, V]
  def extractRegex  (value: java.util.regex.Pattern): ValidationNel[String, V]
  def extractBinary (value: Array[Byte]): ValidationNel[String, V]
  def extractId     (value: ObjectId): ValidationNel[String, V]
  def extractOther  (value: Any) = ("No extractor configured for value of type " + value.getClass.getName).fail.toValidationNel
  def buildNull: ValidationNel[String, V]

  def buildArray  (values: Seq[V]): V
  def buildField  (name: String, value: V): F
  def buildObject (fields: Seq[F]): O

  def apply(dbObject: DBObject): ValidationNel[String, O] = {
    val fields: Seq[ValidationNel[String, F]] = {
      dbObject.keySet.asScala.map(key => fromDBValue(dbObject.get(key)).map(buildField(key, _)))(collection.breakOut)
    }

    fields.toList.sequence[({type N[B] = ValidationNel[String, B]})#N, F].map(buildObject)
  }

  private def fromDBValue(value: Any): ValidationNel[String, V] = value match {
    case x: String                  => extractString(x)
    case x: java.lang.Long          => extractLong(x)
    case x: java.lang.Integer       => extractInteger(x)
    case x: java.lang.Double        => extractDouble(x)
    case x: java.lang.Float         => extractFloat(x)
    case x: java.lang.Boolean       => extractBoolean(x)
    case x: java.util.regex.Pattern => extractRegex(x)
    case x: java.util.Date          => extractDate(x)
    case x: Array[Byte]             => extractBinary(x)
    case x: ObjectId    => extractId(x)
    case x: java.util.ArrayList[_]  => x.asScala.map(fromDBValue).toList.sequence[({type N[B] = ValidationNel[String, B]})#N, V].map(buildArray)
    case x: DBObject                => apply(x)
    case null                       => buildNull
    case other                      => extractOther(other)
  }
}
