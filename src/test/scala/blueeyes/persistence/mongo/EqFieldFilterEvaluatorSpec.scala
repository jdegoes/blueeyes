package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST.JString
import Evaluators._

class EqFieldFilterEvaluatorSpec  extends Specification {
  "returns true for the same JValues" in {
    EqFieldFilterEvaluator(JString("foo"), JString("foo")) must be (true)
  }
  "returns false for different JValues" in {
    EqFieldFilterEvaluator(JString("bar"), JString("foo")) must be (false)
  }
}