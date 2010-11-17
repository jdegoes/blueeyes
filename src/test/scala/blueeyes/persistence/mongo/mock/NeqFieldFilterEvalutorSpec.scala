package blueeyes.persistence.mongo.mock

import org.specs.Specification
import blueeyes.json.JsonAST.JString
import MockMongoFiltersImplementation._

class NeqFieldFilterEvalutorSpec  extends Specification {
  "returns false for the same JValues" in {
    NeFieldFilterEvalutor(JString("foo"), JString("foo")) must be (false)
  }
  "returns true for different JValues" in {
    NeFieldFilterEvalutor(JString("bar"), JString("foo")) must be (true)
  }
}