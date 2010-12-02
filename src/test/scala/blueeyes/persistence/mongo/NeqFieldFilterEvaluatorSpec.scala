package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST.JString
import Evaluators._

class NeqFieldFilterEvaluatorSpec  extends Specification {
  "returns false for the same JValues" in {
    NeFieldFilterEvaluator(JString("foo"), JString("foo")) must be (false)
  }
  "returns true for different JValues" in {
    NeFieldFilterEvaluator(JString("bar"), JString("foo")) must be (true)
  }
}