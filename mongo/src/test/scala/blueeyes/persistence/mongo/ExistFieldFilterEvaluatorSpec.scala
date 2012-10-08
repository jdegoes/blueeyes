package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class ExistFieldFilterEvaluatorSpec  extends Specification {

  "always returns true" in {
    ExistsFieldFilterEvaluator(JArray(JNum(2) :: JNum(3) :: Nil ), JBool(true)) must be_==(true)
    ExistsFieldFilterEvaluator(JNum(4), JBool(true)) must be_==(true)
  }
}