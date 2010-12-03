package blueeyes.persistence.mongo.mock

import org.spex.Specification
import blueeyes.json.JsonAST._
import MockMongoUpdateEvaluators._

class PopFieldEvaluatorSpec  extends Specification{
  "pop first element" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo".popFirst
    PopFieldEvaluator(JArray(JInt(1) :: JInt(2) :: Nil), operation.filter) mustEqual(JArray(JInt(2) :: Nil))
  }
  "pop last element" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo".popLast
    PopFieldEvaluator(JArray(JInt(1) :: JInt(2) :: Nil), operation.filter) mustEqual(JArray(JInt(1) :: Nil))
  }
  "pop empty array" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo".popLast
    PopFieldEvaluator(JArray(Nil), operation.filter) mustEqual(JArray(Nil))
  }
}