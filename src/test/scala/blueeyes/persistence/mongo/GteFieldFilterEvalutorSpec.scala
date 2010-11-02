package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._


class GteFieldFilterEvalutorSpec extends Specification {
  "returns true when one string greater then another string" in {
    GteFieldFilterEvalutor(JString("b"), JString("a")) must be (true)
  }
  "returns false when one string less then another string" in {
    GteFieldFilterEvalutor(JString("a"), JString("b")) must be (false)
  }
  "returns true when one number greater then another number" in {
    GteFieldFilterEvalutor(JInt(2), JInt(1)) must be (true)
  }
  "returns false when one number less then another number" in {
    GteFieldFilterEvalutor(JInt(1), JInt(2)) must be (false)
  }
  "returns true when one double greater then another double" in {
    GteFieldFilterEvalutor(JDouble(2.2), JDouble(1.1)) must be (true)
  }
  "returns false when one double less then another double" in {
    GteFieldFilterEvalutor(JDouble(1.1), JDouble(2.2)) must be (false)
  }
  "returns true when one boolean greater then another boolean" in {
    GteFieldFilterEvalutor(JBool(true), JBool(false)) must be (true)
  }
  "returns false when one boolean less then another boolean" in {
    GteFieldFilterEvalutor(JBool(false), JBool(true)) must be (false)
  }
  "returns false when different object are compared" in {
    GteFieldFilterEvalutor(JBool(false), JInt(1)) must be (false)
  }
  "returns true when objecta are the same" in {
    GteFieldFilterEvalutor(JInt(1), JInt(1)) must be (true)
  }
}