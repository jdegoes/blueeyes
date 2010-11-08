package blueeyes.persistence.mongo

import blueeyes.persistence.mongo.MongoUpdateOperators.MongoUpdateOperator
import MongoUpdateOperators._
import blueeyes.json.JsonAST._
import com.mongodb.MongoException

private[mongo] object MockMongoUpdateEvalutors{
  object UpdateFiledEvalutorFactory{
    def apply(operator: MongoUpdateOperator): UpdateFieldEvalutor = operator match{
      case $inc      => IncFieldEvalutor
      case $set      => SetFieldEvalutor
      case $unset    => UnsetFieldEvalutor
      case $push     => PushFieldEvalutor
      case $pushAll  => PushAllFieldEvalutor
      case $addToSet => AddToSetFieldEvalutor
      case $pop      => PopFieldEvalutor
      case $pull     => PullFieldEvalutor
      case $pullAll  => PullAllFieldEvalutor
    }
  }

  sealed trait UpdateFieldEvalutor extends Function2[JValue, MongoFilter, JValue]{
    def << (filter: MongoFilter) = filter match{
      case e: MongoFieldFilter => e.rhs.toJValue
      case _ => error("filter is not MongoFieldFilter")
    }
  }

  case object SetFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, filter: MongoFilter) = <<(filter)
  }  
  case object IncFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, filter: MongoFilter) = (value, <<(filter)) match {
      case (JInt(x1),     JInt(x2))    => JInt(x1 + x2)
      case (JDouble(x1),  JDouble(x2)) => JDouble(x1 + x2)
      case (JDouble(x1),  JInt(x2))    => JDouble(x1 + x2.doubleValue)
      case (JInt(x1),     JDouble(x2)) => JDouble(x1.doubleValue + x2)
      case _ => throw new MongoException("Modifier $inc allowed for numbers only")       
    }
  }
  case object UnsetFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, filter: MongoFilter) = JNothing
  }
  case object PushFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, filter: MongoFilter) = value match {
      case JNothing  => JArray(<<(filter) :: Nil)
      case JArray(v) => JArray(v :+ <<(filter))
      case _ => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
    }
  }
  case object PushAllFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, filter: MongoFilter) = <<(filter) match {
      case JArray(x) => value match {
        case JNothing  => JArray(x)
        case JArray(v) => JArray(v ++ x)
        case _ => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
      }
      case _ => PushFieldEvalutor(value, filter)
    }
  }
  case object AddToSetFieldEvalutor extends UpdateFieldEvalutor{
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
  case object PopFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, filter: MongoFilter) = value match{
      case JArray(Nil) => value
      case JArray(x)   => <<(filter) match{
        case JInt(y)   => if (y.intValue == 1) JArray(x.dropRight(1)) else if (y.intValue == -1) JArray(x.drop(1)) else throw new MongoException("Cannot apply $pop modifier to non-JInt(1|-1)")  
        case _ => throw new MongoException("Cannot apply $pop modifier to non-JInt(1|-1)")
      }
      case _ => throw new MongoException("Cannot apply $pop modifier to non-array")
    }
  }
  case object PullAllFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, filter: MongoFilter) = (value, <<(filter)) match {
      case (JArray(x), JArray(y)) => JArray(x filterNot (y contains))
      case (JNothing, _) => JNothing
      case _ => throw new MongoException("Cannot apply $pullAll modifier to non-array")
    }
  }  
  case object PullFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, filter: MongoFilter) = value match {
      case e: JArray => value
      case _ => throw new MongoException("Cannot apply pull modifier to non-array")
    }
  }
}