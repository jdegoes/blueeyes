package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json._
import Evaluators._

class EqFieldFilterEvaluatorSpec  extends Specification {
  "returns true for the same JValues" in {
    EqFieldFilterEvaluator(JString("foo"), JString("foo")) must be_==(true)
  }
  "returns false for different JValues" in {
    EqFieldFilterEvaluator(JString("bar"), JString("foo")) must be_==(false)
  }
  "returns true if valie is JUndefined and matching value is JNull" in {
    EqFieldFilterEvaluator(JUndefined, JNull) must be_==(true)
  }
}