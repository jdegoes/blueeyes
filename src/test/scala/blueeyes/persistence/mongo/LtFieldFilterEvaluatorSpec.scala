package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class LtFieldFilterEvaluatorSpec extends Specification {
  "returns false when one string greater then another string" in {
    LtFieldFilterEvaluator(JString("b"), JString("a")) must be (false)
  }
  "returns true when one string less then another string" in {
    LtFieldFilterEvaluator(JString("a"), JString("b")) must be (true)
  }
  "returns false when one number greater then another number" in {
    LtFieldFilterEvaluator(JInt(2), JInt(1)) must be (false)
  }
  "returns true when one number less then another number" in {
    LtFieldFilterEvaluator(JInt(1), JInt(2)) must be (true)
  }
  "returns false when one double greater then another double" in {
    LtFieldFilterEvaluator(JDouble(2.2), JDouble(1.1)) must be (false)
  }
  "returns true when one double less then another double" in {
    LtFieldFilterEvaluator(JDouble(1.1), JDouble(2.2)) must be (true)
  }
  "returns false when one boolean greater then another boolean" in {
    LtFieldFilterEvaluator(JBool(true), JBool(false)) must be (false)
  }
  "returns true when one boolean less then another boolean" in {
    LtFieldFilterEvaluator(JBool(false), JBool(true)) must be (true)
  }
  "returns false when different object are compared" in {
    LtFieldFilterEvaluator(JBool(false), JInt(1)) must be (false)
  }
  "returns false when objecta are the same" in {
    LtFieldFilterEvaluator(JInt(1), JInt(1)) must be (false)
  }
}