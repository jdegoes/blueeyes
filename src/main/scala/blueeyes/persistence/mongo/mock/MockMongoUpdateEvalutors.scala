package blueeyes.persistence.mongo.mock

import blueeyes.persistence.mongo.MongoUpdateOperators.MongoUpdateOperator
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import blueeyes.persistence.mongo._
import blueeyes.persistence.mongo.MongoFilterEvaluator._
import blueeyes.persistence.mongo.MongoUpdateOperators._

private[mock] object MockMongoUpdateEvaluators{
  object UpdateFiledEvaluatorFactory{
    def apply(operator: MongoUpdateOperator): UpdateFieldEvaluator = operator match{
      case $inc      => IncFieldEvaluator
      case $set      => SetFieldEvaluator
      case $unset    => UnsetFieldEvaluator
      case $push     => PushFieldEvaluator
      case $pushAll  => PushAllFieldEvaluator
      case $addToSet => AddToSetFieldEvaluator
      case $pop      => PopFieldEvaluator
      case $pull     => PullFieldEvaluator
      case $pullAll  => PullAllFieldEvaluator
    }
  }

  sealed trait UpdateFieldEvaluator extends Function2[JValue, MongoFilter, JValue]{
    def << (filter: MongoFilter) = filter match{
      case e: MongoFieldFilter => e.rhs.toJValue
      case _ => error("filter is not MongoFieldFilter")
    }
  }

  case object SetFieldEvaluator extends UpdateFieldEvaluator{
    def apply(value: JValue, filter: MongoFilter) = <<(filter)
  }  
  case object IncFieldEvaluator extends UpdateFieldEvaluator{
    def apply(value: JValue, filter: MongoFilter) = (value, <<(filter)) match {
      case (JInt(x1),     JInt(x2))    => JInt(x1 + x2)
      case (JDouble(x1),  JDouble(x2)) => JDouble(x1 + x2)
      case (JDouble(x1),  JInt(x2))    => JDouble(x1 + x2.doubleValue)
      case (JInt(x1),     JDouble(x2)) => JDouble(x1.doubleValue + x2)
      case _ => throw new MongoException("Modifier $inc allowed for numbers only")       
    }
  }
  case object UnsetFieldEvaluator extends UpdateFieldEvaluator{
    def apply(value: JValue, filter: MongoFilter) = JNothing
  }
  case object PushFieldEvaluator extends UpdateFieldEvaluator{
    def apply(value: JValue, filter: MongoFilter) = value match {
      case JNothing  => JArray(<<(filter) :: Nil)
      case JArray(v) => JArray(v :+ <<(filter))
      case _ => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
    }
  }
  case object PushAllFieldEvaluator extends UpdateFieldEvaluator{
    def apply(value: JValue, filter: MongoFilter) = <<(filter) match {
      case JArray(x) => value match {
        case JNothing  => JArray(x)
        case JArray(v) => JArray(v ++ x)
        case _ => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
      }
      case _ => PushFieldEvaluator(value, filter)
    }
  }
  case object AddToSetFieldEvaluator extends UpdateFieldEvaluator{
    def apply(value: JValue, filter: MongoFilter) = value match {
      case JNothing  => <<(filter) match {
        case e: JArray => e
        case JNothing  => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
        case _  => JArray(<<(filter) :: Nil)
      }
      case e: JArray => <<(filter) match {
        case y: JArray => y.elements.foldLeft(e){(result, element) => addToSet(result, element)}
        case JNothing  => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
        case _  => addToSet(e, <<(filter))
      }
      case _ => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
    }

    private def addToSet(value: JArray, setValue: JValue) = value.find(_ == setValue) match {
      case Some(x) => value
      case None => JArray(value.elements :+ setValue)
    }
  }
  case object PopFieldEvaluator extends UpdateFieldEvaluator{
    def apply(value: JValue, filter: MongoFilter) = value match{
      case JArray(Nil) => value
      case JArray(x)   => <<(filter) match{
        case JInt(y)   => if (y.intValue == 1) JArray(x.dropRight(1)) else if (y.intValue == -1) JArray(x.drop(1)) else throw new MongoException("Cannot apply $pop modifier to non-JInt(1|-1)")  
        case _ => throw new MongoException("Cannot apply $pop modifier to non-JInt(1|-1)")
      }
      case _ => throw new MongoException("Cannot apply $pop modifier to non-array")
    }
  }
  case object PullAllFieldEvaluator extends UpdateFieldEvaluator{
    def apply(value: JValue, filter: MongoFilter) = (value, <<(filter)) match {
      case (JArray(x), JArray(y)) => JArray(x filterNot (y contains))
      case (JNothing, _) => JNothing
      case _ => throw new MongoException("Cannot apply $pullAll modifier to non-array")
    }
  }  
  case object PullFieldEvaluator extends UpdateFieldEvaluator{
    def apply(value: JValue, filter: MongoFilter) = value match {
      case JArray(x) => JArray(x filterNot (x.filter(filter) contains) )
      case _ => throw new MongoException("Cannot apply pull modifier to non-array")
    }
  }
}