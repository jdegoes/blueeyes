package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json._
import Evaluators._

class AllFieldFilterEvaluatorSpec  extends Specification {
  "returns true when not all elemenets matched" in {
    AllFieldFilterEvaluator(JArray(JNum(2) :: JNum(3) :: Nil ), JArray(JNum(1) :: JNum(2) :: JNum(3) :: Nil )) must be_==(true)
  }
  "returns false when all elemenets matched" in {
    AllFieldFilterEvaluator(JArray(JNum(2) :: JNum(3) :: JNum(4) :: Nil ), JArray(JNum(1) :: JNum(2) :: JNum(3) :: Nil )) must be_==(false)
  }
}