package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class ModFieldFilterEvaluatorSpec  extends Specification {
  "returns true when int modular devision is true" in {
    ModFieldFilterEvaluator(JInt(11), JArray(JInt(10) :: JInt(1) :: Nil )) must be (true)
  }
  "returns true when double modular devision is true" in {
    ModFieldFilterEvaluator(JDouble(11.0), JArray(JInt(10) :: JInt(1) :: Nil )) must be (true)
  }
  "returns false when int modular devision is false" in {
    ModFieldFilterEvaluator(JInt(21), JArray(JInt(10) :: JInt(2) :: Nil )) must be (false)
  }
}