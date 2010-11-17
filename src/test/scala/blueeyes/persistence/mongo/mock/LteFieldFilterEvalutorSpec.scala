package blueeyes.persistence.mongo.mock

import org.specs.Specification
import blueeyes.json.JsonAST._
import MockMongoFiltersImplementation._

class LteFieldFilterEvalutorSpec extends Specification {
  "returns false when one string greater then another string" in {
    LteFieldFilterEvalutor(JString("b"), JString("a")) must be (false)
  }
  "returns true when one string less then another string" in {
    LteFieldFilterEvalutor(JString("a"), JString("b")) must be (true)
  }
  "returns false when one number greater then another number" in {
    LteFieldFilterEvalutor(JInt(2), JInt(1)) must be (false)
  }
  "returns true when one number less then another number" in {
    LteFieldFilterEvalutor(JInt(1), JInt(2)) must be (true)
  }
  "returns false when one double greater then another double" in {
    LteFieldFilterEvalutor(JDouble(2.2), JDouble(1.1)) must be (false)
  }
  "returns true when one double less then another double" in {
    LteFieldFilterEvalutor(JDouble(1.1), JDouble(2.2)) must be (true)
  }
  "returns false when one boolean greater then another boolean" in {
    LteFieldFilterEvalutor(JBool(true), JBool(false)) must be (false)
  }
  "returns true when one boolean less then another boolean" in {
    LteFieldFilterEvalutor(JBool(false), JBool(true)) must be (true)
  }
  "returns false when different object are compared" in {
    LteFieldFilterEvalutor(JBool(false), JInt(1)) must be (false)
  }
  "returns true when objecta are the same" in {
    LteFieldFilterEvalutor(JInt(1), JInt(1)) must be (true)
  }  
}