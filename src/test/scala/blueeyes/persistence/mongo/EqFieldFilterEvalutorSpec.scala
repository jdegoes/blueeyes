package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST.JString
import MockMongoFiltersImplementation._

class EqFieldFilterEvalutorSpec  extends Specification {
  "returns true for the same JValues" in {
    EqFieldFilterEvalutor(JString("foo"), JString("foo")) must be (true)
  }
  "returns false for different JValues" in {
    EqFieldFilterEvalutor(JString("bar"), JString("foo")) must be (false)
  }
}