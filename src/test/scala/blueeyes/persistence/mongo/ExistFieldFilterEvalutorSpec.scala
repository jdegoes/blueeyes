package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._

class ExistFieldFilterEvalutorSpec  extends Specification {

  "always returns true" in {
    ExistsFieldFilterEvalutor(JArray(JInt(2) :: JInt(3) :: Nil ), JBool(true)) must be (true)
    ExistsFieldFilterEvalutor(JInt(4), JBool(true)) must be (true)
  }
}