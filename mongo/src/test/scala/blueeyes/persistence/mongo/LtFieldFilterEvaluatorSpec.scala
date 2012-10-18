package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json._
import Evaluators._

class LtFieldFilterEvaluatorSpec extends Specification {
  "returns false when one string greater then another string" in {
    LtFieldFilterEvaluator(JString("b"), JString("a")) must be_==(false)
  }
  "returns true when one string less then another string" in {
    LtFieldFilterEvaluator(JString("a"), JString("b")) must be_==(true)
  }
  "returns false when one number greater then another number" in {
    LtFieldFilterEvaluator(JNum(2), JNum(1)) must be_==(false)
  }
  "returns true when one number less then another number" in {
    LtFieldFilterEvaluator(JNum(1), JNum(2)) must be_==(true)
  }
  "returns false when one double greater then another double" in {
    LtFieldFilterEvaluator(JNum(2.2), JNum(1.1)) must be_==(false)
  }
  "returns true when one double less then another double" in {
    LtFieldFilterEvaluator(JNum(1.1), JNum(2.2)) must be_==(true)
  }
  "returns false when one boolean greater then another boolean" in {
    LtFieldFilterEvaluator(JBool(true), JBool(false)) must be_==(false)
  }
  "returns true when one boolean less then another boolean" in {
    LtFieldFilterEvaluator(JBool(false), JBool(true)) must be_==(true)
  }
  "returns false when different object are compared" in {
    LtFieldFilterEvaluator(JBool(false), JNum(1)) must be_==(false)
  }
  "returns false when objecta are the same" in {
    LtFieldFilterEvaluator(JNum(1), JNum(1)) must be_==(false)
  }
}