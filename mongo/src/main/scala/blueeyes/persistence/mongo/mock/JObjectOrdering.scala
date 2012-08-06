package blueeyes.persistence.mongo.mock

import blueeyes.json.JPath
import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo._
import blueeyes.persistence.mongo.Evaluators._
import blueeyes.util.GeoTools

private[mock] object JObjectOrderingFactory extends MongoFilters{
  def apply(filter: Option[MongoFilter], sort: Option[MongoSort]): Option[Ordering[JObject]] = {
    sort.map(v => JObjectOrdering(v.sortField, v.sortOrder.order): Ordering[JObject]).orElse{filter.flatMap{ filterValue =>
      nearFilter(filterValue) match {
        case Some(MongoFieldFilter(lhs, _, MongoPrimitiveJObject(JObject(JField(_, JArray(List(JNum(nearX), JNum(nearY)))) :: xs)))) => Some(GeoFieldOrdering(lhs, nearX.doubleValue, nearY.doubleValue): Ordering[JObject])
        case _ => None
      }
    }}
  }
}

private[mongo] case class GeoFieldOrdering(path: JPath, x: Double, y: Double) extends Ordering[JObject] with GeoTools{
  private def compareDistance(x1: Double, y1: Double, x2: Double, y2: Double) = distanceInMiles(x1.doubleValue(), y1.doubleValue(), x, y) - distanceInMiles(x2.doubleValue(), y2.doubleValue(), x, y) match{
    case 0 => 0
    case x => if (x < 0) -1 else 1
  }
  def compare(o1: JObject, o2: JObject) = (normalizeGeoField(o1.get(path)), normalizeGeoField(o2.get(path))) match {
    case (JArray(JNum(x1) :: JNum(y1) :: xs), JArray(JNum(x2) :: JNum(y2) :: ys))                        => compareDistance(x1.doubleValue, y1.doubleValue, x2.doubleValue, y2.doubleValue)
    case (JArray(JNum(x1) :: JNum(y1) :: xs), JObject(JField(_, JNum(x2)) :: JField(_, JNum(y2)) :: ys)) => compareDistance(x1.doubleValue, y1.doubleValue, x2.doubleValue, y2.doubleValue)

    case (JObject(JField(_, JNum(x1)) :: JField(_, JNum(y1)) :: xs), JArray(JNum(x2) :: JNum(y2) :: ys))                        => compareDistance(x1.doubleValue, y1.doubleValue, x2.doubleValue, y2.doubleValue)
    case (JObject(JField(_, JNum(x1)) :: JField(_, JNum(y1)) :: xs), JObject(JField(_, JNum(x2)) :: JField(_, JNum(y2)) :: ys)) => compareDistance(x1.doubleValue, y1.doubleValue, x2.doubleValue, y2.doubleValue)

    case _ => -1
  }
}

private[mongo] case class JObjectOrdering(path: JPath, weight: Int) extends Ordering[JObject]{
  def compare(o1: JObject, o2: JObject) = (o1.get(path), o2.get(path)) match {
    case (JString(x1),  JString(x2)) => x1.compare(x2) * weight
    case (JNum(x1),     JNum(x2))    => x1.compare(x2) * weight
    case (JBool(x1),    JBool(x2))   => x1.compare(x2) * weight
    case (JNull,        JNull)       => 0
    case (v,            JNull)       => 1
    case (JNull,        v)           => -1
    case (JNothing,     JNothing)    => 0
    case (v,            JNothing)       => 1
    case (JNothing,     v)           => -1
    case _ => sys.error("differents elements cannot be ordered")
  }
}