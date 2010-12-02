package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class ExistFieldFilterEvaluatorSpec  extends Specification {

  "always returns true" in {
    ExistsFieldFilterEvaluator(JArray(JInt(2) :: JInt(3) :: Nil ), JBool(true)) must be (true)
    ExistsFieldFilterEvaluator(JInt(4), JBool(true)) must be (true)
  }
}