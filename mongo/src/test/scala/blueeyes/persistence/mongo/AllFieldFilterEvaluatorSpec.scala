package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class AllFieldFilterEvaluatorSpec  extends Specification {
  "returns true when not all elemenets matched" in {
    AllFieldFilterEvaluator(JArray(JInt(2) :: JInt(3) :: Nil ), JArray(JInt(1) :: JInt(2) :: JInt(3) :: Nil )) must be_==(true)
  }
  "returns false when all elemenets matched" in {
    AllFieldFilterEvaluator(JArray(JInt(2) :: JInt(3) :: JInt(4) :: Nil ), JArray(JInt(1) :: JInt(2) :: JInt(3) :: Nil )) must be_==(false)
  }
}