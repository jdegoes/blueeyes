package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.persistence.mongo.MongoFilterOperators._
import Evaluators._

class FieldFilterEvaluatorFactorySpec extends Specification {
  "creates $eq Evaluator" in {
    FieldFilterEvaluatorFactory($eq) must be (EqFieldFilterEvaluator)
  }
  "creates $ne Evaluator" in {
    FieldFilterEvaluatorFactory($ne) must be (NeFieldFilterEvaluator)
  }
  "creates $gt Evaluator" in {
    FieldFilterEvaluatorFactory($gt) must be (GtFieldFilterEvaluator)
  }
  "creates $gte Evaluator" in {
    FieldFilterEvaluatorFactory($gte) must be (GteFieldFilterEvaluator)
  }
  "creates $lt Evaluator" in {
    FieldFilterEvaluatorFactory($lt) must be (LtFieldFilterEvaluator)
  }
  "creates $lte Evaluator" in {
    FieldFilterEvaluatorFactory($lte) must be (LteFieldFilterEvaluator)
  }
  "creates $in Evaluator" in {
    FieldFilterEvaluatorFactory($in) must be (InFieldFilterEvaluator)
  }
  "creates $nin Evaluator" in {
    FieldFilterEvaluatorFactory($nin) must be (NinFieldFilterEvaluator)
  }
  "creates $mod Evaluator" in {
    FieldFilterEvaluatorFactory($mod) must be (ModFieldFilterEvaluator)
  }
  "creates $all Evaluator" in {
    FieldFilterEvaluatorFactory($all) must be (AllFieldFilterEvaluator)
  }

  "creates $size Evaluator" in {
    FieldFilterEvaluatorFactory($size) must be (SizeFieldFilterEvaluator)
  }
  "creates $exists Evaluator" in {
    FieldFilterEvaluatorFactory($exists) must be (ExistsFieldFilterEvaluator)
  }
  "creates $type Evaluator" in {
    FieldFilterEvaluatorFactory($type) must be (TypeFieldFilterEvaluator)
  }
}