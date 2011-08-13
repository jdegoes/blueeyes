package blueeyes.persistence.mongo

import blueeyes.json.JsonAST._
import MongoFilterOperators._
import blueeyes.json.JPath
import util.matching.Regex
import blueeyes.util.GeoTools
import com.mongodb.MongoException

private[mongo] object MongoFilterEvaluator{
  implicit def valuesToEvaluator(values: List[JValue]) = MongoFilterEvaluator(values)
}

private[mongo] case class MongoFilterEvaluator(values: List[JValue]){
  import Evaluators._
  
  def filter(filter: MongoFilter):  List[JValue] = filter match{
    case MongoFilterAll               => values
    case x : MongoFieldFilter         => MongoFieldFilterEvaluator(values, x)
    case x : MongoOrFilter            => MongoOrFilterEvaluator   (values, x)
    case x : MongoAndFilter           => MongoAndFilterEvaluator  (values, x)
    case x : MongoElementsMatchFilter => MongoElementsMatchFilterEvaluator(values, x)
  }
}
private[mongo] object Evaluators{
  import MongoFilterEvaluator._

  object MongoElementsMatchFilterEvaluator{
    def apply(values: List[JValue], filter: MongoElementsMatchFilter) = {
      values.filter(value => searchElements(value.get(filter.lhs), filter))
    }
    private def searchElements(element: JValue, filter: MongoElementsMatchFilter) = element match{
      case JArray(x)    =>
          val matched = x.filter(element => element match{
          case e: JObject => !MongoAndFilterEvaluator(e :: Nil, filter.elementsQuery).isEmpty
          case _ => false
        })
        !matched.isEmpty
      case JNothing => false
      case _        => !MongoAndFilterEvaluator(element :: Nil, filter.elementsQuery).isEmpty
    }
  }

  object MongoOrFilterEvaluator{
    def apply(values: List[JValue], filter: MongoOrFilter) = filter.queries.foldLeft(List[JValue]()){ (result, currentFilter) => result.union(values.filter(currentFilter)) }
  }

  object MongoAndFilterEvaluator{
    def apply(values: List[JValue], filter: MongoAndFilter) = filter.queries.toList match{
      case x :: xs => xs.foldLeft(values.filter(x)){ (result, currentFilter) => result.filter(currentFilter) }
      case Nil     => values
    }
  }

  object MongoFieldFilterEvaluator{
    def apply(values: List[JValue], filter: MongoFieldFilter) = searchByField(values, filter)

    private def searchByField(values: List[JValue], filter: MongoFieldFilter) = {
      val evaluator = FieldFilterEvaluatorFactory(filter.lhs, filter.operator)
      values.filter(value => {
        val pathValue = value.get(filter.lhs)
        !pathValue.filter(v => {
          v match{
            case JArray(x) => evaluator(v, filter.rhs.toJValue) || x.foldLeft(false){(result, element) => result || evaluator(element, filter.rhs.toJValue) }
            case _         => evaluator(v, filter.rhs.toJValue)
          }
        }).isEmpty
      })
    }
  }

  object FieldFilterEvaluatorFactory{
    def apply(lhs: JPath, operator: MongoFilterOperator): FieldFilterEvaluator = operator match{
      case $gt          => GtFieldFilterEvaluator
      case $gte         => GteFieldFilterEvaluator
      case $lt          => LtFieldFilterEvaluator
      case $lte         => LteFieldFilterEvaluator
      case $eq          => EqFieldFilterEvaluator
      case $ne          => NeFieldFilterEvaluator
      case $in          => InFieldFilterEvaluator
      case $nin         => NinFieldFilterEvaluator
      case $mod         => ModFieldFilterEvaluator
      case $all         => AllFieldFilterEvaluator
      case $size        => SizeFieldFilterEvaluator
      case $exists      => ExistsFieldFilterEvaluator
      case $type        => TypeFieldFilterEvaluator
      case $regex       => RegexFilterEvaluator
      case $near        => NearFilterEvaluator
      case $nearSphere  => NearSphereFilterEvaluator
      case $within      => WithinFilterEvaluator
      case $or          => sys.error("'or' is not supported")
      case $each        => sys.error("'or' is not supported")
    }
  }

  sealed trait FieldFilterEvaluator extends Function2[JValue, JValue, Boolean]

  case object EqFieldFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = v1 == v2 || v2 == JNull && v1 == JNothing
  }
  case object NeFieldFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = {
      if (v1 == JNothing) v2 != JNull else v1 != v2
    }
  }
  case object GtFieldFilterEvaluator extends FieldFilterEvaluator{
    import Predef._
    def apply(v1: JValue, v2: JValue) = (v1, v2) match {
      case (JString(x1),  JString(x2)) => augmentString(x1) > augmentString(x2)
      case (JInt(x1),     JInt(x2))    => x1 > x2
      case (JDouble(x1),  JDouble(x2)) => x1 > x2
      case (JDouble(x1),  JInt(x2))    => x1 > x2.doubleValue
      case (JInt(x1),     JDouble(x2)) => x1.doubleValue > x2
      case (JBool(x1),    JBool(x2))   => x1 > x2
      case _ => false
    }
  }
  case object GteFieldFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = EqFieldFilterEvaluator(v1, v2) || GtFieldFilterEvaluator(v1, v2)
  }
  case object LtFieldFilterEvaluator extends FieldFilterEvaluator{
    import Predef._
    def apply(v1: JValue, v2: JValue) = (v1, v2) match {
      case (JString(x1),  JString(x2)) => augmentString(x1) < augmentString(x2)
      case (JInt(x1),     JInt(x2))    => x1 < x2
      case (JDouble(x1),  JDouble(x2)) => x1 < x2
      case (JDouble(x1),  JInt(x2))    => x1 < x2.doubleValue
      case (JInt(x1),     JDouble(x2)) => x1.doubleValue < x2
      case (JBool(x1),    JBool(x2))   => x1 < x2
      case _ => false
    }
  }
  case object LteFieldFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = EqFieldFilterEvaluator(v1, v2) || LtFieldFilterEvaluator(v1, v2)
  }
  case object InFieldFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = v2 match {
      case JArray(x)  => x.exists(_ == v1)
      case JObject(x) => x.exists(_ == v1)
      case _ => false
    }
  }
  case object NinFieldFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = v2 match {
      case JArray(x)  => !x.exists(_ == v1)
      case JObject(x) => !x.exists(_ == v1)
      case _ => false
    }
  }
  case object ModFieldFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = (v1, v2) match {
      case (JInt(x1),     JArray(JInt(y1) :: JInt(y2) :: Nil))    => x1 % y1 == y2
      case (JDouble(x1),  JArray(JInt(y1) :: JInt(y2) :: Nil))    => x1 % y1.toDouble == y2
      case _ => false
    }
  }
  case object AllFieldFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = (v1, v2) match {
      case (JArray(x1), JArray(x2)) => (x1 filterNot(x2 contains) ).isEmpty
      case _ => false
    }
  }
  case object SizeFieldFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = (v1, v2) match {
      case (JArray(x1), JInt(x2)) => x1.size == x2
      case _ => false
    }
  }
  case object ExistsFieldFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = v1 != JNothing
  }
  case object RegexFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = (v1, v2) match {
      case (JString(value), JObject(List(JField("$regex", JString(regexValue)), JField("$options", JString(options))))) =>
        val regex = regexValue match{
          case "" => regexValue
          case _ => "(?%s)%s".format(options, regexValue)
        }
        new Regex(regex) findPrefixMatchOf (value) match {
          case Some(x) => true
          case None => false
        }
      case _ => false
    }
  }

  case object TypeFieldFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = v2 match {
      case JInt(x) => (v1, x.intValue()) match{
        case (JString(_), 2)  => true
        case (JDouble(_), 1)  => true
        case (JObject(_), 3)  => true
        case (JArray(_),  4)  => true
        case (JBool(_),   8)  => true
        case (JNull,      10) => true
        case (JInt(_),    18) => true
        case (JInt(_),    1) => true
        case _ => false
      }
      case _ => false
    }
  }

  trait NearFilterEvaluatorBase extends FieldFilterEvaluator with GeoTools{
    def apply(v1: JValue, v2: JValue) = {
      def evaluate(x: Double, y: Double) = v2 match{
        case JObject(JField(query, JArray(List(JDouble(nearX), JDouble(nearY)))) :: Nil) => true
        case JObject(JField(query, JArray(List(JDouble(nearX), JDouble(nearY)))) :: JField("$maxDistance", JDouble(maxDistance)) :: Nil) => isNear(nearX, nearY, x, y, maxDistance)
        case _ => false
      }
      normalizeGeoField(v1) match {
        case (JArray(JDouble(x) :: JDouble(y) :: xs))                        => evaluate(x, y)
        case (JObject(JField(_, JDouble(x)) :: JField(_, JDouble(y)) :: xs)) => evaluate(x, y)
        case _ => false
      }
    }
    private def isNear(nearX: Double, nearY: Double, x: Double, y: Double, maxDistance: Double) = distanceInMiles(nearX, nearY, x, y) <= realDistance(maxDistance)

    protected def realDistance(distance: Double): Double
    protected def query: String
  }

  case object NearFilterEvaluator extends NearFilterEvaluatorBase{
    protected val query = "$near"
    protected def realDistance(distance: Double) = distance
  }

  case object NearSphereFilterEvaluator extends NearFilterEvaluatorBase{
    protected val query = "$nearSphere"
    protected def realDistance(distance: Double) = distance * earthRadiusInMiles
  }

  case object WithinFilterEvaluator extends FieldFilterEvaluator with GeoTools{
    def apply(v1: JValue, v2: JValue) = {
      def extractPoints(points: List[JValue]) = points.collect{case JArray(JDouble(x) :: JDouble(y) :: Nil) => (x, y)}
      def evaluate(x: Double, y: Double) = v2 match{
        case JObject(JField("$within", JObject(JField("$box",     JArray(JArray(JDouble(lowerLeftX) :: JDouble(lowerLeftY) :: Nil) :: JArray(JDouble(upperRightX) :: JDouble(upperRightY) :: Nil) :: Nil)) :: Nil)) :: Nil) => x >= lowerLeftX && x <= upperRightX && y >= lowerLeftY && y <= upperRightY
        case JObject(JField("$within", JObject(JField("$center",  JArray(JArray(JDouble(centerX) :: JDouble(centerY) :: Nil) :: JDouble(radius) :: Nil)) :: Nil)) :: Nil) => distanceInMiles(centerX, centerY, x, y) <= radius
        case JObject(JField("$within", JObject(JField("$centerSphere",  JArray(JArray(JDouble(centerX) :: JDouble(centerY) :: Nil) :: JDouble(radiusInRadians) :: Nil)) :: Nil)) :: Nil) =>
          if (radiusInRadians > 0.492) throw new MongoException(13462, "Spherical distance would require wrapping, which isn't implemented yet")
          distanceInMiles(centerX, centerY, x, y) <= (radiusInRadians * earthRadiusInMiles)
        case JObject(JField("$within", JObject(JField("$polygon", JArray(points)) :: Nil)) :: Nil) => inPolygon((x, y), extractPoints(points).toArray)
        case _ => false
      }
      normalizeGeoField(v1) match {
        case (JArray(JDouble(x) :: JDouble(y) :: xs))                        => evaluate(x, y)
        case (JObject(JField(_, JDouble(x)) :: JField(_, JDouble(y)) :: xs)) => evaluate(x, y)
        case _ => false
      }
    }
  }

  def normalizeGeoField(v: JValue) = v match{
    case JArray(elements) => JArray(elements.map{element => element match {
      case JInt(x) => JDouble(x.toDouble)
      case _ => element
    }})
    case JObject(fields) => JObject(fields.map{field => field match{
      case JField(name, JInt(x)) => JField(name, JDouble(x.toDouble))
      case _ => field
    }})
    case _ => v
  }

  def inPolygon(point: (Double, Double), polygon: Array[(Double, Double)]): Boolean = {
    var oddNodes = false

    var j = polygon.length - 1
    for (i <- 0 to polygon.length - 1)
    {
      if (polygon(i)._2 < point._2 && polygon(j)._2 >= point._2 || polygon(j)._2 < point._2 && polygon(i)._2 >= point._2)
        if (polygon(i)._1 + (point._2 - polygon(i)._2)/(polygon(j)._2 - polygon(i)._2)*(polygon(j)._1 - polygon(i)._1) < point._1)
          oddNodes = !oddNodes
      j = i
    }

    oddNodes
  }
}