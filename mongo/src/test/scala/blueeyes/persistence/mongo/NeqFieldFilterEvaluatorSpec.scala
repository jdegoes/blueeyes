package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json._
import Evaluators._
import blueeyes.json.JPathImplicits

class NeqFieldFilterEvaluatorSpec  extends Specification with JPathImplicits{
  "returns false for the same JValues" in {
    NeFieldFilterEvaluator(JString("foo"), JString("foo")) must be_==(false)
  }
  "returns true for different JValues" in {
    NeFieldFilterEvaluator(JString("bar"), JString("foo")) must be_==(true)
  }
  "returns false if value is JUndefined and match value is null" in {
    NeFieldFilterEvaluator(JUndefined, JNull) must be_==(false)
  }
  "returns true if value is JUndefined and match value is not null" in {
    NeFieldFilterEvaluator(JUndefined, JString("bar")) must be_==(true)
  }
}