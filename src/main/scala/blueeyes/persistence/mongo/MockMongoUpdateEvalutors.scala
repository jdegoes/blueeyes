package blueeyes.persistence.mongo

import blueeyes.persistence.mongo.MongoUpdateOperators.MongoUpdateOperator
import MongoUpdateOperators._
import blueeyes.json.JsonAST.{JInt, JDouble, JObject, JValue}
import com.mongodb.MongoException

private[mongo] object MockMongoUpdateEvalutors{
  object UpdateFiledEvalutorFactory{
    def apply(operator: MongoUpdateOperator) = operator match{
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

  sealed trait UpdateFieldEvalutor extends Function2[JValue, JValue, JValue]

  case object SetFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, setValue: JValue) = setValue
  }  
  case object IncFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, setValue: JValue) = (value, setValue) match {
      case (JInt(x1),     JInt(x2))    => JInt(x1 + x2)
      case (JDouble(x1),  JDouble(x2)) => JDouble(x1 + x2)
      case (JDouble(x1),  JInt(x2))    => JDouble(x1 + x2.doubleValue)
      case (JInt(x1),     JDouble(x2)) => JDouble(x1.doubleValue + x2)
      case _ => throw new MongoException("Modifier $inc allowed for numbers only")       
    }
  }
  case object UnsetFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, setValue: JValue) = value
  }
  case object PushFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, setValue: JValue) = value
  }
  case object PushAllFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, setValue: JValue) = value
  }
  case object AddToSetFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, setValue: JValue) = value
  }
  case object PopFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, setValue: JValue) = value
  }
  case object PullFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, setValue: JValue) = value
  }
  case object PullAllFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JValue, setValue: JValue) = value
  }

}