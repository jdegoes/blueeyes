package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class SizeFieldFilterEvaluatorSpec  extends Specification {
  "returns true when size is correct" in {
    SizeFieldFilterEvaluator(JArray(JInt(2) :: JInt(3) :: Nil ), JInt(2)) must be (true)
  }
  "returns true when size is not correct" in {
    SizeFieldFilterEvaluator(JArray(JInt(2) :: JInt(3) :: JInt(4) :: Nil ), JInt(4)) must be (false)
  }
}