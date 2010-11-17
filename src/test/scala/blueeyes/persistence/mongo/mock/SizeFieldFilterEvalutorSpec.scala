package blueeyes.persistence.mongo.mock

import org.specs.Specification
import blueeyes.json.JsonAST._
import MockMongoFiltersImplementation._

class SizeFieldFilterEvalutorSpec  extends Specification {
  "returns true when size is correct" in {
    SizeFieldFilterEvalutor(JArray(JInt(2) :: JInt(3) :: Nil ), JInt(2)) must be (true)
  }
  "returns true when size is not correct" in {
    SizeFieldFilterEvalutor(JArray(JInt(2) :: JInt(3) :: JInt(4) :: Nil ), JInt(4)) must be (false)
  }
}