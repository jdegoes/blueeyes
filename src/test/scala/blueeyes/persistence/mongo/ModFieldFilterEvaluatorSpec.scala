package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class ModFieldFilterEvaluatorSpec  extends Specification {
  "returns true when int modular devision is true" in {
    ModFieldFilterEvaluator(JInt(11), JArray(JInt(10) :: JInt(1) :: Nil )) must be_==(true)
  }
  "returns true when double modular devision is true" in {
    ModFieldFilterEvaluator(JDouble(11.0), JArray(JInt(10) :: JInt(1) :: Nil )) must be_==(true)
  }
  "returns false when int modular devision is false" in {
    ModFieldFilterEvaluator(JInt(21), JArray(JInt(10) :: JInt(2) :: Nil )) must be_==(false)
  }
}