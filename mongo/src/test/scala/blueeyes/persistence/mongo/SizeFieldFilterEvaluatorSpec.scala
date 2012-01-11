package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class SizeFieldFilterEvaluatorSpec  extends Specification {
  "returns true when size is correct" in {
    SizeFieldFilterEvaluator(JArray(JInt(2) :: JInt(3) :: Nil ), JInt(2)) must be_==(true)
  }
  "returns true when size is not correct" in {
    SizeFieldFilterEvaluator(JArray(JInt(2) :: JInt(3) :: JInt(4) :: Nil ), JInt(4)) must be_==(false)
  }
}