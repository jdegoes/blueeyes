package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class InFieldFilterEvaluatorSpec extends Specification {
  "returns true when value in array" in {
    InFieldFilterEvaluator(JString("b"), JArray(JString("b") :: JString("a") :: Nil)) must be (true)
  }
  "returns false when value in not array" in {
    InFieldFilterEvaluator(JString("b"), JArray(JString("c") :: JString("a") :: Nil)) must be (false)
  }
}