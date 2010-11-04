package blueeyes.persistence.mongo

import MongoUpdateOperators._
import MockMongoUpdateEvalutors._
import org.spex.Specification

class UpdateFiledEvalutorFactorySpec extends Specification {
  "creates $inc Evalutor" in {
    UpdateFiledEvalutorFactory($inc) must be (IncFieldEvalutor)
  }
  "creates $set Evalutor" in {
    UpdateFiledEvalutorFactory($set) must be (SetFieldEvalutor)
  }
  "creates $unset Evalutor" in {
    UpdateFiledEvalutorFactory($unset) must be (UnsetFieldEvalutor)
  }
  "creates $push Evalutor" in {
    UpdateFiledEvalutorFactory($push) must be (PushFieldEvalutor)
  }
  "creates $pushAll Evalutor" in {
    UpdateFiledEvalutorFactory($pushAll) must be (PushAllFieldEvalutor)
  }
  "creates $addToSet Evalutor" in {
    UpdateFiledEvalutorFactory($addToSet) must be (AddToSetFieldEvalutor)
  }
  "creates $pop Evalutor" in {
    UpdateFiledEvalutorFactory($pop) must be (PopFieldEvalutor)
  }
  "creates $pull Evalutor" in {
    UpdateFiledEvalutorFactory($pull) must be (PullFieldEvalutor)
  }
  "creates $pullAll Evalutor" in {
    UpdateFiledEvalutorFactory($pullAll) must be (PullAllFieldEvalutor)
  }
}