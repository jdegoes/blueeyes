package blueeyes.persistence.mongo

import blueeyes.json.JsonAST._
import MongoFilterOperators._
import util.matching.Regex
import blueeyes.util.GeoTools
import com.mongodb.MongoException
import blueeyes.js.RhinoScript
import blueeyes.json.{Printer, JPath}

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
    def apply(values: List[JValue], filter: MongoOrFilter) = {
      val uniqueFilter = filter.queries.distinct
      values.filter{jvo => uniqueFilter.foldLeft(false){(result, filter) =>
        result || (List(jvo).filter(filter) match{
          case x :: xs => true
          case Nil     => false
        })
      }}
    }
  }

  object MongoAndFilterEvaluator{
    def apply(values: List[JValue], filter: MongoAndFilter) = filter.queries.distinct.toList match{
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
        pathValue match{
          case JArray(x) => evaluator(pathValue, filter.rhs.toJValue) || x.foldLeft(false){(result, element) => result || evaluator(element, filter.rhs.toJValue) }
          case _         => evaluator(pathValue, filter.rhs.toJValue)
        }
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
      case $where       => WhereFilterEvaluator
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
      case (JNum(x1),     JNum(x2))    => x1 > x2
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
      case (JNum(x1),     JNum(x2))    => x1 < x2
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
      case (JNum(x1),     JArray(JNum(y1) :: JNum(y2) :: Nil))    => x1 % y1 == y2
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
      case (JArray(x1), JNum(x2)) => x1.size == x2
      case _ => false
    }
  }
  case object ExistsFieldFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = v2 match{
      case JBool(value) => if (value) v1 != JNothing else v1 == JNothing
      case _ => false
    }
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
      case JNum(x) => (v1, x.intValue()) match{
        case (JString(_), 2)  => true
        case (JNum(_), 1)  => true
        case (JObject(_), 3)  => true
        case (JArray(_),  4)  => true
        case (JBool(_),   8)  => true
        case (JNull,      10) => true
        case (JNum(_),    18) => true
        case _ => false
      }
      case _ => false
    }
  }

  trait NearFilterEvaluatorBase extends FieldFilterEvaluator with GeoTools{
    def apply(v1: JValue, v2: JValue) = {
      def evaluate(x: Double, y: Double) = v2 match{
        case JObject(JField(query, JArray(List(JNum(nearX), JNum(nearY))))) => true
        case JObject(JField(query, JArray(List(JNum(nearX), JNum(nearY)))), JField("$maxDistance", JNum(maxDistance))) => 
          isNear(nearX.doubleValue, nearY.doubleValue, x.doubleValue, y.doubleValue, maxDistance.doubleValue)
        case _ => false
      }
      normalizeGeoField(v1) match {
        case (JArray(JNum(x) :: JNum(y) :: xs))                        => evaluate(x.doubleValue, y.doubleValue)
        case (JObject(JField(_, JNum(x)) :: JField(_, JNum(y)) :: xs)) => evaluate(x.doubleValue, y.doubleValue)
        case _ => false
      }
    }
    private def isNear(nearX: Double, nearY: Double, x: Double, y: Double, maxDistance: Double) = {
      val inMiles = distanceInMiles(nearX, nearY, x, y)
      val distance = realDistance(maxDistance)
      inMiles <= distance
    }

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
      def extractPoints(points: List[JValue]) = points.collect{case JArray(JNum(x) :: JNum(y) :: Nil) => (x.doubleValue, y.doubleValue)}
      def evaluate(x: Double, y: Double) = v2 match{
        case JObject(JField("$within", JObject(JField("$box",     JArray(JArray(JNum(lowerLeftX) :: JNum(lowerLeftY) :: Nil) :: JArray(JNum(upperRightX) :: JNum(upperRightY) :: Nil) :: Nil)) :: Nil)) :: Nil) => x >= lowerLeftX && x <= upperRightX && y >= lowerLeftY && y <= upperRightY
        case JObject(JField("$within", JObject(JField("$center",  JArray(JArray(JNum(centerX) :: JNum(centerY) :: Nil) :: JNum(radius) :: Nil)) :: Nil)) :: Nil) => distanceInMiles(centerX.doubleValue, centerY.doubleValue, x, y) <= radius
        case JObject(JField("$within", JObject(JField("$centerSphere",  JArray(JArray(JNum(centerX) :: JNum(centerY) :: Nil) :: JNum(radiusInRadians) :: Nil)) :: Nil)) :: Nil) =>
          if (radiusInRadians > 0.492) throw new MongoException(13462, "Spherical distance would require wrapping, which isn't implemented yet")
          distanceInMiles(centerX.doubleValue, centerY.doubleValue, x, y) <= (radiusInRadians * earthRadiusInMiles)
        case JObject(JField("$within", JObject(JField("$polygon", JArray(points)) :: Nil)) :: Nil) => inPolygon((x.doubleValue, y.doubleValue), extractPoints(points).toArray)
        case _ => false
      }
      normalizeGeoField(v1) match {
        case (JArray(JNum(x) :: JNum(y) :: xs))                        => evaluate(x.doubleValue, y.doubleValue)
        case (JObject(JField(_, JNum(x)) :: JField(_, JNum(y)) :: xs)) => evaluate(x.doubleValue, y.doubleValue)
        case _ => false
      }
    }
  }

  case object WhereFilterEvaluator extends FieldFilterEvaluator with WhereFilterScriptBuilder{
    def apply(v1: JValue, v2: JValue) = (v1, v2) match{
      case (e: JObject, JString(function)) =>
        RhinoScript(build(function, e))().map{_ match{
          case JBool(v)   => v
          case JNum(v)    => v != 0
          case JString(v) => v.length != 0
          case JObject(v) => true
          case JArray(v)  => true
          case _          => false
        }}.getOrElse(false)
      case _ => false
    }
  }

  private[mongo] trait WhereFilterScriptBuilder {
    private val scriptPattern = """var obj = %s; obj.evaluate = %s; obj.evaluate()"""
    def build(function: String, value: JObject) = {
      val trimmed = function.trim
      val fullFunction = if (trimmed.startsWith("function")) function
      else if (trimmed.startsWith("{")) {
        if (trimmed.indexOf("return") != -1) "function()" + function
        else throw new MongoException(10070, "$where compile error")
      }
      else if (trimmed.startsWith("return")) "function(){" + function + "}"
      else "function(){return " + function + "}"
      scriptPattern.format(Printer.compact(Printer.render(value)), fullFunction)
    }
  }

  def normalizeGeoField(v: JValue) = v

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