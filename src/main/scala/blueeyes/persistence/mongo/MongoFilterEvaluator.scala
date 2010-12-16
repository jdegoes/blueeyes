package blueeyes.persistence.mongo

import blueeyes.json.JsonAST._
import MongoFilterOperators._

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
      values.filter(value => !searchElements(value.get(filter.lhs), filter).isEmpty)
    }
    private def searchElements(elements: List[JValue], filter: MongoElementsMatchFilter) = {
      elements.filter(v => v match {
        case JArray(x)    => 
            val matched = x.filter(element => element match{
            case e: JObject => !MongoAndFilterEvaluator(e :: Nil, filter.elementsQuery).isEmpty
            case _ => false
          })
          !matched.isEmpty
        case e: JObject => !MongoAndFilterEvaluator(e :: Nil, filter.elementsQuery).isEmpty
        case _          => false
      })
    }
  }

  object MongoOrFilterEvaluator{
    def apply(values: List[JValue], filter: MongoOrFilter) = filter.queries.foldLeft(List[JValue]()){ (result, currentFilter) => result.union(values.filter(currentFilter)) }
  }

  object MongoAndFilterEvaluator{
    def apply(values: List[JValue], filter: MongoAndFilter) = filter.queries match{
      case x :: xs => xs.foldLeft(values.filter(x)){ (result, currentFilter) =>  result.intersect(values.filter(currentFilter)) }
      case Nil     => values
    }
  }

  object MongoFieldFilterEvaluator{
    def apply(values: List[JValue], filter: MongoFieldFilter) = searchByField(values, filter)

    private def searchByField(values: List[JValue], filter: MongoFieldFilter) = {
      val evaluator = FieldFilterEvaluatorFactory(filter.operator)
      values.filter(value => {
        val pathValue = value.get(filter.lhs)
        !pathValue.filter(v => {
          v match{
            case JArray(x) => x.foldLeft(false){(result, element) => result || evaluator(element, filter.rhs.toJValue) }
            case _         => evaluator(v, filter.rhs.toJValue)
          }
        }).isEmpty
      })
    }
  }

  object FieldFilterEvaluatorFactory{
    def apply(operator: MongoFilterOperator): FieldFilterEvaluator = operator match{
      case $gt      => GtFieldFilterEvaluator
      case $gte     => GteFieldFilterEvaluator
      case $lt      => LtFieldFilterEvaluator
      case $lte     => LteFieldFilterEvaluator
      case $eq      => EqFieldFilterEvaluator
      case $ne      => NeFieldFilterEvaluator
      case $in      => InFieldFilterEvaluator
      case $nin     => NinFieldFilterEvaluator
      case $mod     => ModFieldFilterEvaluator
      case $all     => AllFieldFilterEvaluator
      case $size    => SizeFieldFilterEvaluator
      case $exists  => ExistsFieldFilterEvaluator
      case $type    => TypeFieldFilterEvaluator
      case $or      => error("'or' is not supported")
      case $each    => error("'or' is not supported")
    }
  }

  sealed trait FieldFilterEvaluator extends Function2[JValue, JValue, Boolean]

  case object EqFieldFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = v1 == v2
  }
  case object NeFieldFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = v1 != v2
  }
  case object GtFieldFilterEvaluator extends FieldFilterEvaluator{
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
  case object GteFieldFilterEvaluator extends FieldFilterEvaluator{
    def apply(v1: JValue, v2: JValue) = EqFieldFilterEvaluator(v1, v2) || GtFieldFilterEvaluator(v1, v2)
  }
  case object LtFieldFilterEvaluator extends FieldFilterEvaluator{
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
    def apply(v1: JValue, v2: JValue) = true
  }
  case object TypeFieldFilterEvaluator extends FieldFilterEvaluator{
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