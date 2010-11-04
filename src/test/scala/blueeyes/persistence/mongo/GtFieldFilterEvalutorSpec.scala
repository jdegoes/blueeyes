package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import MockMongoFiltersEvalutors._


class GtFieldFilterEvalutorSpec extends Specification {
  "returns true when one string greater then another string" in {
    GtFieldFilterEvalutor(JString("b"), JString("a")) must be (true)
  }
  "returns false when one string less then another string" in {
    GtFieldFilterEvalutor(JString("a"), JString("b")) must be (false)
  }
  "returns true when one number greater then another number" in {
    GtFieldFilterEvalutor(JInt(2), JInt(1)) must be (true)
  }
  "returns false when one number less then another number" in {
    GtFieldFilterEvalutor(JInt(1), JInt(2)) must be (false)
  }
  "returns true when one double greater then another double" in {
    GtFieldFilterEvalutor(JDouble(2.2), JDouble(1.1)) must be (true)
  }
  "returns false when one double less then another double" in {
    GtFieldFilterEvalutor(JDouble(1.1), JDouble(2.2)) must be (false)
  }
  "returns true when one boolean greater then another boolean" in {
    GtFieldFilterEvalutor(JBool(true), JBool(false)) must be (true)
  }
  "returns false when one boolean less then another boolean" in {
    GtFieldFilterEvalutor(JBool(false), JBool(true)) must be (false)
  }
  "returns false when different object are compared" in {
    GtFieldFilterEvalutor(JBool(false), JInt(1)) must be (false)
  }
  "returns false when objecta are the same" in {
    GtFieldFilterEvalutor(JInt(1), JInt(1)) must be (false)
  }
  
}