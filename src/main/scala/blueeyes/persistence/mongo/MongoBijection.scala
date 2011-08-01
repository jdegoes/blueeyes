package blueeyes.persistence.mongo

import scala.collection.JavaConversions._
import org.joda.time.DateTime
import com.mongodb.{BasicDBObject, DBObject}
import java.util.regex.Pattern
import blueeyes.core.data.Bijection

class MongoBijection[V <: MongoValueRepr, F, O <: V {type Values = Map[String, Any]}](factory: MongoBijectionFactory[V, F, O]) extends Bijection[DBObject, O]{

  implicit def apply(dbObject: DBObject): O = {
    val allKeys  = asScalaSet(dbObject.keySet)
    val pureKeys = allKeys.filter(_ != "_id")

    def toJField(key: String) = factory.mongoField(key, fromDBValue(dbObject.get(key)))

    factory.mongoObject(pureKeys.map(toJField(_)).toList)
  }

  implicit def unapply(mongoObject: O): DBObject = toDBObject(mongoObject.values)

  private def fromDBValue[M](value: M): V = value match {
    case x: String                  => factory.mongoString(x)
    case x: java.lang.Long          => factory.mongoLong(x)
    case x: java.lang.Integer       => factory.mongoInteger(x)
    case x: java.lang.Double        => factory.mongoDouble(x)
    case x: java.lang.Float         => factory.mongoFloat(x)
    case x: java.lang.Boolean       => factory.mongoBoolean(x)
    case x: java.util.ArrayList[_]  => factory.mongoArray(x.map(fromDBValue).toList)
    case x: java.util.regex.Pattern => factory.mongoRegex(x.pattern, if (x.flags != 0) Some(x.flags) else None)
    case x: java.util.Date          => factory.mongoDate(new DateTime(x))
    case x: Array[Byte]             => factory.mongoBinary(x)
    case x: DBObject                => apply(x)
    case null                       => factory.mongoNull
    // Missing cases: com.mongodb.ObjectId, com.mongodb.DBRef
    case _                               => sys.error("Unknown type for. {value=" + value + "}")
  }

  private def toDBObject(values: Map[String, Any]): DBObject = new BasicDBObject(values.mapValues(toDBValue(_)).filter(_._2 != None).mapValues(_.get))

  private def toDBValue(value: Any): Option[Any] = {
    import Predef._

    value match {
      case x: String      => Some(x.value)
      case x: BigInt      => Some(long2Long(x.longValue))
      case x: Int         => Some(int2Integer(x))
      case x: Long        => Some(long2Long(x))
      case x: Double      => Some(double2Double(x))
      case x: Float       => Some(float2Float(x))
      case x: Boolean     => Some(boolean2Boolean(x))
      case x: Array[Byte] => Some(x)
      case x: DateTime    => Some(x.toDate)
      case (pattern: String, option: Int)   => Some(Pattern.compile(pattern, option))
      case x: Map[_, _]   => Some(toDBObject(Map(x.toSeq.map(el => (el._1.toString, el._2)): _*)))
      case x: List[_]     => {
        val values = new java.util.ArrayList[Any]
        x.foreach(y => {
          toDBValue(y) match {
            case Some(z) => values.add(z)
            case _ =>
          }
        })
        Some(values)
      }
      case None   => None
      case null      => Some(null)
      case _          => sys.error("Unknown type for value: " + value)
    }
  }
}

trait MongoBijectionFactory[V <: MongoValueRepr, F, O <: V {type Values = Map[String, Any]}]{
  def mongoBinary (value: Array[Byte]): V
  def mongoDate   (value: DateTime): V
  def mongoRegex  (pattern: String, flags: Option[Int]): V
  def mongoString (value: String): V
  def mongoLong   (value: Long): V
  def mongoInteger(value: Int): V
  def mongoDouble (value: Double): V
  def mongoFloat  (value: Float): V
  def mongoBoolean(value: Boolean): V
  def mongoArray  (value: List[V]): V
  def mongoNull: V
  def mongoField  (name: String, value: V): F
  def mongoObject (fields: List[F]): O
}