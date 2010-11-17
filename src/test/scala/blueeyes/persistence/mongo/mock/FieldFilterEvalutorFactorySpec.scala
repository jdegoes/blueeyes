package blueeyes.persistence.mongo.mock

import org.specs.Specification
import blueeyes.persistence.mongo.MongoFilterOperators._
import MockMongoFiltersImplementation._

class FieldFilterEvalutorFactorySpec extends Specification {
  "creates $eq Evalutor" in {
    FieldFilterEvalutorFactory($eq) must be (EqFieldFilterEvalutor)
  }
  "creates $ne Evalutor" in {
    FieldFilterEvalutorFactory($ne) must be (NeFieldFilterEvalutor)
  }
  "creates $gt Evalutor" in {
    FieldFilterEvalutorFactory($gt) must be (GtFieldFilterEvalutor)
  }
  "creates $gte Evalutor" in {
    FieldFilterEvalutorFactory($gte) must be (GteFieldFilterEvalutor)
  }
  "creates $lt Evalutor" in {
    FieldFilterEvalutorFactory($lt) must be (LtFieldFilterEvalutor)
  }
  "creates $lte Evalutor" in {
    FieldFilterEvalutorFactory($lte) must be (LteFieldFilterEvalutor)
  }
  "creates $in Evalutor" in {
    FieldFilterEvalutorFactory($in) must be (InFieldFilterEvalutor)
  }
  "creates $nin Evalutor" in {
    FieldFilterEvalutorFactory($nin) must be (NinFieldFilterEvalutor)
  }
  "creates $mod Evalutor" in {
    FieldFilterEvalutorFactory($mod) must be (ModFieldFilterEvalutor)
  }
  "creates $all Evalutor" in {
    FieldFilterEvalutorFactory($all) must be (AllFieldFilterEvalutor)
  }

  "creates $size Evalutor" in {
    FieldFilterEvalutorFactory($size) must be (SizeFieldFilterEvalutor)
  }
  "creates $exists Evalutor" in {
    FieldFilterEvalutorFactory($exists) must be (ExistsFieldFilterEvalutor)
  }
  "creates $type Evalutor" in {
    FieldFilterEvalutorFactory($type) must be (TypeFieldFilterEvalutor)
  }
}