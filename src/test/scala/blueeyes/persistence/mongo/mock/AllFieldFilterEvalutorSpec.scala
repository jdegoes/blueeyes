package blueeyes.persistence.mongo.mock

import org.specs.Specification
import blueeyes.json.JsonAST._
import MockMongoFiltersImplementation._

class AllFieldFilterEvalutorSpec  extends Specification {
  "returns true when not all elemenets matched" in {
    AllFieldFilterEvalutor(JArray(JInt(2) :: JInt(3) :: Nil ), JArray(JInt(1) :: JInt(2) :: JInt(3) :: Nil )) must be (true)
  }
  "returns false when all elemenets matched" in {
    AllFieldFilterEvalutor(JArray(JInt(2) :: JInt(3) :: JInt(4) :: Nil ), JArray(JInt(1) :: JInt(2) :: JInt(3) :: Nil )) must be (false)
  }
}