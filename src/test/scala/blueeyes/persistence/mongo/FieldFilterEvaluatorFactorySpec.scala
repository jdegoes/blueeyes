package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.persistence.mongo.MongoFilterOperators._
import Evaluators._
import blueeyes.json.JPathImplicits

class FieldFilterEvaluatorFactorySpec extends Specification with JPathImplicits{
  "creates $eq Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $eq) must be (EqFieldFilterEvaluator)
  }
  "creates $ne Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $ne) must be (NeFieldFilterEvaluator)
  }
  "creates $gt Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $gt) must be (GtFieldFilterEvaluator)
  }
  "creates $gte Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $gte) must be (GteFieldFilterEvaluator)
  }
  "creates $lt Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $lt) must be (LtFieldFilterEvaluator)
  }
  "creates $lte Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $lte) must be (LteFieldFilterEvaluator)
  }
  "creates $in Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $in) must be (InFieldFilterEvaluator)
  }
  "creates $nin Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $nin) must be (NinFieldFilterEvaluator)
  }
  "creates $mod Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $mod) must be (ModFieldFilterEvaluator)
  }
  "creates $all Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $all) must be (AllFieldFilterEvaluator)
  }

  "creates $size Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $size) must be (SizeFieldFilterEvaluator)
  }
  "creates $exists Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $exists) must be (ExistsFieldFilterEvaluator)
  }
  "creates $type Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $type) must be (TypeFieldFilterEvaluator)
  }
  "creates $regex Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $regex) must be (RegexFilterEvaluator)
  }
  "creates $nearSphere Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $nearSphere) must be (NearSphereFilterEvaluator)
  }
  "creates $near Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $near) must be (NearFilterEvaluator)
  }
  "creates $within Evaluator" in {
    FieldFilterEvaluatorFactory("foo", $within) must be (WithinFilterEvaluator)
  }
}