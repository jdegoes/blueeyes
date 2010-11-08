package blueeyes.persistence.mongo

import org.spex.Specification
import blueeyes.json.JsonAST._
import MockMongoUpdateEvalutors._
import blueeyes.json.JPathImplicits._
import MongoImplicits._

class PopFieldEvalutorSpec  extends Specification{
  "pop first element" in {
    import MongoFilterImplicits._

    val operation = "foo".popFirst
    PopFieldEvalutor(JArray(JInt(1) :: JInt(2) :: Nil), operation.filter) mustEqual(JArray(JInt(2) :: Nil))
  }
  "pop last element" in {
    import MongoFilterImplicits._

    val operation = "foo".popLast
    PopFieldEvalutor(JArray(JInt(1) :: JInt(2) :: Nil), operation.filter) mustEqual(JArray(JInt(1) :: Nil))
  }
  "pop empty array" in {
    import MongoFilterImplicits._

    val operation = "foo".popLast
    PopFieldEvalutor(JArray(Nil), operation.filter) mustEqual(JArray(Nil))
  }
}