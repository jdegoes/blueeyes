package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json._
import Evaluators._

class ModFieldFilterEvaluatorSpec  extends Specification {
  "returns true when int modular devision is true" in {
    ModFieldFilterEvaluator(JNum(11), JArray(JNum(10) :: JNum(1) :: Nil )) must be_==(true)
  }
  "returns true when double modular devision is true" in {
    ModFieldFilterEvaluator(JNum(11.0), JArray(JNum(10) :: JNum(1) :: Nil )) must be_==(true)
  }
  "returns false when int modular devision is false" in {
    ModFieldFilterEvaluator(JNum(21), JArray(JNum(10) :: JNum(2) :: Nil )) must be_==(false)
  }
}