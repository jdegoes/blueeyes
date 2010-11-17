package blueeyes.persistence.mongo.mock

import org.spex.Specification
import blueeyes.json.JsonAST._
import MockMongoUpdateEvalutors._
import blueeyes.json.JPathImplicits._
import blueeyes.persistence.mongo.MongoImplicits._

class PopFieldEvalutorSpec  extends Specification{
  "pop first element" in {
    import blueeyes.persistence.mongo.MongoFilterImplicits._

    val operation = "foo".popFirst
    PopFieldEvalutor(JArray(JInt(1) :: JInt(2) :: Nil), operation.filter) mustEqual(JArray(JInt(2) :: Nil))
  }
  "pop last element" in {
    import blueeyes.persistence.mongo.MongoFilterImplicits._

    val operation = "foo".popLast
    PopFieldEvalutor(JArray(JInt(1) :: JInt(2) :: Nil), operation.filter) mustEqual(JArray(JInt(1) :: Nil))
  }
  "pop empty array" in {
    import blueeyes.persistence.mongo.MongoFilterImplicits._

    val operation = "foo".popLast
    PopFieldEvalutor(JArray(Nil), operation.filter) mustEqual(JArray(Nil))
  }
}