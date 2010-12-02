package blueeyes.persistence.mongo.mock

import blueeyes.persistence.mongo.MongoUpdateOperators._
import MockMongoUpdateEvaluators._
import org.spex.Specification

class UpdateFiledEvaluatorFactorySpec extends Specification {
  "creates $inc Evaluator" in {
    UpdateFiledEvaluatorFactory($inc) must be (IncFieldEvaluator)
  }
  "creates $set Evaluator" in {
    UpdateFiledEvaluatorFactory($set) must be (SetFieldEvaluator)
  }
  "creates $unset Evaluator" in {
    UpdateFiledEvaluatorFactory($unset) must be (UnsetFieldEvaluator)
  }
  "creates $push Evaluator" in {
    UpdateFiledEvaluatorFactory($push) must be (PushFieldEvaluator)
  }
  "creates $pushAll Evaluator" in {
    UpdateFiledEvaluatorFactory($pushAll) must be (PushAllFieldEvaluator)
  }
  "creates $addToSet Evaluator" in {
    UpdateFiledEvaluatorFactory($addToSet) must be (AddToSetFieldEvaluator)
  }
  "creates $pop Evaluator" in {
    UpdateFiledEvaluatorFactory($pop) must be (PopFieldEvaluator)
  }
  "creates $pull Evaluator" in {
    UpdateFiledEvaluatorFactory($pull) must be (PullFieldEvaluator)
  }
  "creates $pullAll Evaluator" in {
    UpdateFiledEvaluatorFactory($pullAll) must be (PullAllFieldEvaluator)
  }
}