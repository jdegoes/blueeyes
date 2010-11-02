package blueeyes.persistence.mongo

import org.specs.Specification
import MongoFilterOperators._

class FieldFilterEvalutorsSpec extends Specification {
  "creates $eq Evalutor" in {
    FieldFilterEvalutors($eq) must be (EqFieldFilterEvalutor)
  }
  "creates $ne Evalutor" in {
    FieldFilterEvalutors($ne) must be (NeFieldFilterEvalutor)
  }
  "creates $gt Evalutor" in {
    FieldFilterEvalutors($gt) must be (GtFieldFilterEvalutor)
  }
  "creates $gte Evalutor" in {
    FieldFilterEvalutors($gte) must be (GteFieldFilterEvalutor)
  }
  "creates $lt Evalutor" in {
    FieldFilterEvalutors($lt) must be (LtFieldFilterEvalutor)
  }
  "creates $lte Evalutor" in {
    FieldFilterEvalutors($lte) must be (LteFieldFilterEvalutor)
  }
  "creates $in Evalutor" in {
    FieldFilterEvalutors($in) must be (InFieldFilterEvalutor)
  }
  "creates $nin Evalutor" in {
    FieldFilterEvalutors($nin) must be (NinFieldFilterEvalutor)
  }
  "creates $mod Evalutor" in {
    FieldFilterEvalutors($mod) must be (ModFieldFilterEvalutor)
  }
  "creates $all Evalutor" in {
    FieldFilterEvalutors($all) must be (AllFieldFilterEvalutor)
  }

  "creates $size Evalutor" in {
    FieldFilterEvalutors($size) must be (SizeFieldFilterEvalutor)
  }
  "creates $exists Evalutor" in {
    FieldFilterEvalutors($exists) must be (ExistsFieldFilterEvalutor)
  }
  "creates $type Evalutor" in {
    FieldFilterEvalutors($type) must be (TypeFieldFilterEvalutor)
  }
}