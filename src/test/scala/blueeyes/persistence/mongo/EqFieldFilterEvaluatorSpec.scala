package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class EqFieldFilterEvaluatorSpec  extends Specification {
  "returns true for the same JValues" in {
    EqFieldFilterEvaluator(JString("foo"), JString("foo")) must be (true)
  }
  "returns false for different JValues" in {
    EqFieldFilterEvaluator(JString("bar"), JString("foo")) must be (false)
  }
  "returns true if valie is JNothing and matching value is JNull" in {
    EqFieldFilterEvaluator(JNothing, JNull) must be (true)
  }
}