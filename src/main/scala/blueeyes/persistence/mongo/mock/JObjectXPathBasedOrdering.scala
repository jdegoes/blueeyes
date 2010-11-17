package blueeyes.persistence.mongo.mock

import blueeyes.json.JsonAST._
import blueeyes.json.{JPath}

private[mongo] class JObjectXPathBasedOrdering(path: JPath, weight: Int) extends Ordering[JObject]{
  def compare(o1: JObject, o2: JObject) = (o1.get(path), o2.get(path)) match {
    case (v1 :: Nil, v2 :: Nil) =>
      (v1, v2) match {
        case (JString(x1),  JString(x2)) => x1.compare(x2) * weight
        case (JInt(x1),     JInt(x2))    => x1.compare(x2) * weight
        case (JDouble(x1),  JDouble(x2)) => x1.compare(x2) * weight
        case (JDouble(x1),  JInt(x2))    => x1.compare(x2.doubleValue) * weight
        case (JInt(x1),     JDouble(x2)) => x1.doubleValue.compare(x2) * weight
        case (JBool(x1),    JBool(x2))   => x1.compare(x2) * weight
        case (JNull,        JNull)       => 0
        case (v,            JNull)       => 1
        case (JNull,        v)           => -1
        case (JNothing,     JNothing)    => 0
        case (v,            JNothing)       => 1
        case (JNothing,     v)           => -1
        case _ => error("differents elements cannot be ordered")
      }
    case _ => error("lists cannot be ordered")
  }
}
