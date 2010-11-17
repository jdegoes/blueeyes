package blueeyes.persistence.mongo.mock

import org.specs.Specification
import blueeyes.json.JsonAST._
import MockMongoFiltersImplementation._

class NinFieldFilterEvalutorSpec extends Specification {
  "returns false when value in array" in {
    NinFieldFilterEvalutor(JString("b"), JArray(JString("b") :: JString("a") :: Nil)) must be (false)
  }
  "returns true when value in not array" in {
    NinFieldFilterEvalutor(JString("b"), JArray(JString("c") :: JString("a") :: Nil)) must be (true)
  }
}