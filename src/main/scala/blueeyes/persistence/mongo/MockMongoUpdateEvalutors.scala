package blueeyes.persistence.mongo

import blueeyes.persistence.mongo.MongoUpdateOperators.MongoUpdateOperator
import blueeyes.json.JsonAST.{JObject, JValue}
import MongoUpdateOperators._

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

  sealed trait UpdateFieldEvalutor extends Function2[JObject, JValue, JValue]

  case object IncFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JObject, setValue: JValue) = value
  }
  case object SetFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JObject, setValue: JValue) = value
  }
  case object UnsetFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JObject, setValue: JValue) = value
  }
  case object PushFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JObject, setValue: JValue) = value
  }
  case object PushAllFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JObject, setValue: JValue) = value
  }
  case object AddToSetFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JObject, setValue: JValue) = value
  }
  case object PopFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JObject, setValue: JValue) = value
  }
  case object PullFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JObject, setValue: JValue) = value
  }
  case object PullAllFieldEvalutor extends UpdateFieldEvalutor{
    def apply(value: JObject, setValue: JValue) = value
  }

}