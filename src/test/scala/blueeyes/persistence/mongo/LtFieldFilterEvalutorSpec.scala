package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._


class LtFieldFilterEvalutorSpec extends Specification {
  "returns false when one string greater then another string" in {
    LtFieldFilterEvalutor(JString("b"), JString("a")) must be (false)
  }
  "returns true when one string less then another string" in {
    LtFieldFilterEvalutor(JString("a"), JString("b")) must be (true)
  }
  "returns false when one number greater then another number" in {
    LtFieldFilterEvalutor(JInt(2), JInt(1)) must be (false)
  }
  "returns true when one number less then another number" in {
    LtFieldFilterEvalutor(JInt(1), JInt(2)) must be (true)
  }
  "returns false when one double greater then another double" in {
    LtFieldFilterEvalutor(JDouble(2.2), JDouble(1.1)) must be (false)
  }
  "returns true when one double less then another double" in {
    LtFieldFilterEvalutor(JDouble(1.1), JDouble(2.2)) must be (true)
  }
  "returns false when one boolean greater then another boolean" in {
    LtFieldFilterEvalutor(JBool(true), JBool(false)) must be (false)
  }
  "returns true when one boolean less then another boolean" in {
    LtFieldFilterEvalutor(JBool(false), JBool(true)) must be (true)
  }
  "returns false when different object are compared" in {
    LtFieldFilterEvalutor(JBool(false), JInt(1)) must be (false)
  }
  "returns false when objecta are the same" in {
    LtFieldFilterEvalutor(JInt(1), JInt(1)) must be (false)
  }
}