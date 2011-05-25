package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import Evaluators._
import blueeyes.json.JPathImplicits

class NeqFieldFilterEvaluatorSpec  extends Specification with JPathImplicits{
  "returns false for the same JValues" in {
    NeFieldFilterEvaluator("foo.bar")(JString("foo"), JString("foo")) must be (false)
  }
  "returns true for different JValues" in {
    NeFieldFilterEvaluator("foo.bar")(JString("bar"), JString("foo")) must be (true)
  }
  "returns true if path has index and values are different" in {
    NeFieldFilterEvaluator("foo[0]")(JString("bar"), JString("foo")) must be (true)
  }
  "returns false if path last element is index and matching value is JNull" in {
    NeFieldFilterEvaluator("foo[0]")(JString("bar"), JNull) must be (false)
  }
  "returns true if path not last element is index and matching value is JNull" in {
    NeFieldFilterEvaluator("foo[0].bar")(JString("bar"), JNull) must be (true)
  }
}