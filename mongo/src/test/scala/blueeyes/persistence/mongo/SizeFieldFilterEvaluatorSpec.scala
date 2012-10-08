package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class SizeFieldFilterEvaluatorSpec  extends Specification {
  "returns true when size is correct" in {
    SizeFieldFilterEvaluator(JArray(JNum(2) :: JNum(3) :: Nil ), JNum(2)) must be_==(true)
  }
  "returns true when size is not correct" in {
    SizeFieldFilterEvaluator(JArray(JNum(2) :: JNum(3) :: JNum(4) :: Nil ), JNum(4)) must be_==(false)
  }
}