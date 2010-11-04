package blueeyes.persistence.mongo

import blueeyes.persistence.mongo.MongoFilterOperators.MongoFilterOperator
import blueeyes.json.JsonAST._
import MongoFilterOperators._

private[mongo] object MockMongoFiltersEvalutors{
  object FieldFilterEvalutorFactory{
    def apply(operator: MongoFilterOperator): FieldFilterEvalutor = operator match{
      case $gt      => GtFieldFilterEvalutor
      case $gte     => GteFieldFilterEvalutor
      case $lt      => LtFieldFilterEvalutor
      case $lte     => LteFieldFilterEvalutor
      case $eq      => EqFieldFilterEvalutor
      case $ne      => NeFieldFilterEvalutor
      case $in      => InFieldFilterEvalutor
      case $nin     => NinFieldFilterEvalutor
      case $mod     => ModFieldFilterEvalutor
      case $all     => AllFieldFilterEvalutor
      case $size    => SizeFieldFilterEvalutor
      case $exists  => ExistsFieldFilterEvalutor
      case $type    => TypeFieldFilterEvalutor
      case $or      => error("'or' is not supported")
    }
  }

  sealed trait FieldFilterEvalutor extends Function2[JValue, JValue, Boolean]

  case object EqFieldFilterEvalutor extends FieldFilterEvalutor{
    def apply(v1: JValue, v2: JValue) = v1 == v2
  }
  case object NeFieldFilterEvalutor extends FieldFilterEvalutor{
    def apply(v1: JValue, v2: JValue) = v1 != v2
  }
  case object GtFieldFilterEvalutor extends FieldFilterEvalutor{
    def apply(v1: JValue, v2: JValue) = (v1, v2) match {
      case (JString(x1),  JString(x2)) => x1 > x2
      case (JInt(x1),     JInt(x2))    => x1 > x2
      case (JDouble(x1),  JDouble(x2)) => x1 > x2
      case (JDouble(x1),  JInt(x2))    => x1 > x2.doubleValue
      case (JInt(x1),     JDouble(x2)) => x1.doubleValue > x2
      case (JBool(x1),    JBool(x2))   => x1 > x2
      case _ => false
    }
  }
  case object GteFieldFilterEvalutor extends FieldFilterEvalutor{
    def apply(v1: JValue, v2: JValue) = EqFieldFilterEvalutor(v1, v2) || GtFieldFilterEvalutor(v1, v2)
  }
  case object LtFieldFilterEvalutor extends FieldFilterEvalutor{
    def apply(v1: JValue, v2: JValue) = (v1, v2) match {
      case (JString(x1),  JString(x2)) => x1 < x2
      case (JInt(x1),     JInt(x2))    => x1 < x2
      case (JDouble(x1),  JDouble(x2)) => x1 < x2
      case (JDouble(x1),  JInt(x2))    => x1 < x2.doubleValue
      case (JInt(x1),     JDouble(x2)) => x1.doubleValue < x2
      case (JBool(x1),    JBool(x2))   => x1 < x2
      case _ => false
    }
  }
  case object LteFieldFilterEvalutor extends FieldFilterEvalutor{
    def apply(v1: JValue, v2: JValue) = EqFieldFilterEvalutor(v1, v2) || LtFieldFilterEvalutor(v1, v2)
  }
  case object InFieldFilterEvalutor extends FieldFilterEvalutor{
    def apply(v1: JValue, v2: JValue) = v2 match {
      case JArray(x)  => x.exists(_ == v1)
      case JObject(x) => x.exists(_ == v1)
      case _ => false
    }
  }
  case object NinFieldFilterEvalutor extends FieldFilterEvalutor{
    def apply(v1: JValue, v2: JValue) = v2 match {
      case JArray(x)  => !x.exists(_ == v1)
      case JObject(x) => !x.exists(_ == v1)
      case _ => false
    }
  }
  case object ModFieldFilterEvalutor extends FieldFilterEvalutor{
    def apply(v1: JValue, v2: JValue) = (v1, v2) match {
      case (JInt(x1),     JArray(JInt(y1) :: JInt(y2) :: Nil))    => x1 % y1 == y2
      case (JDouble(x1),  JArray(JInt(y1) :: JInt(y2) :: Nil))    => x1 % y1.toDouble == y2
      case _ => false
    }
  }
  case object AllFieldFilterEvalutor extends FieldFilterEvalutor{
    def apply(v1: JValue, v2: JValue) = (v1, v2) match {
      case (JArray(x1), JArray(x2)) => (x1 filterNot(x2 contains) ).isEmpty
      case _ => false
    }
  }
  case object SizeFieldFilterEvalutor extends FieldFilterEvalutor{
    def apply(v1: JValue, v2: JValue) = (v1, v2) match {
      case (JArray(x1), JInt(x2)) => x1.size == x2
      case _ => false
    }
  }
  case object ExistsFieldFilterEvalutor extends FieldFilterEvalutor{
    def apply(v1: JValue, v2: JValue) = true
  }
  case object TypeFieldFilterEvalutor extends FieldFilterEvalutor{
    def apply(v1: JValue, v2: JValue) = v2 match {
      case JInt(x) => (v1, x.intValue) match{
        case (JString(_), 2)  => true
        case (JDouble(_), 1)  => true
        case (JObject(_), 3)  => true
        case (JArray(_),  4)  => true
        case (JBool(_),   8)  => true
        case (JNull,      10) => true
        case (JInt(_),    18) => true
        case _ => false
      }
      case _ => false
    }
  }
}