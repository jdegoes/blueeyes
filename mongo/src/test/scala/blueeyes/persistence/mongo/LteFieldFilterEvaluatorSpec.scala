package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class LteFieldFilterEvaluatorSpec extends Specification {
  "returns false when one string greater then another string" in {
    LteFieldFilterEvaluator(JString("b"), JString("a")) must be_==(false)
  }
  "returns true when one string less then another string" in {
    LteFieldFilterEvaluator(JString("a"), JString("b")) must be_==(true)
  }
  "returns false when one number greater then another number" in {
    LteFieldFilterEvaluator(JInt(2), JInt(1)) must be_==(false)
  }
  "returns true when one number less then another number" in {
    LteFieldFilterEvaluator(JInt(1), JInt(2)) must be_==(true)
  }
  "returns false when one double greater then another double" in {
    LteFieldFilterEvaluator(JDouble(2.2), JDouble(1.1)) must be_==(false)
  }
  "returns true when one double less then another double" in {
    LteFieldFilterEvaluator(JDouble(1.1), JDouble(2.2)) must be_==(true)
  }
  "returns false when one boolean greater then another boolean" in {
    LteFieldFilterEvaluator(JBool(true), JBool(false)) must be_==(false)
  }
  "returns true when one boolean less then another boolean" in {
    LteFieldFilterEvaluator(JBool(false), JBool(true)) must be_==(true)
  }
  "returns false when different object are compared" in {
    LteFieldFilterEvaluator(JBool(false), JInt(1)) must be_==(false)
  }
  "returns true when objecta are the same" in {
    LteFieldFilterEvaluator(JInt(1), JInt(1)) must be_==(true)
  }  
}