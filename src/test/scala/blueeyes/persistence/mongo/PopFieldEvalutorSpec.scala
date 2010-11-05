package blueeyes.persistence.mongo

import org.spex.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvalutors._

class PopFieldEvalutorSpec  extends Specification{
  "pop first element" in {
    PopFieldEvalutor(JArray(JInt(1) :: JInt(2) :: Nil), JInt(-1)) mustEqual(JArray(JInt(2) :: Nil))
  }
  "pop last element" in {
    PopFieldEvalutor(JArray(JInt(1) :: JInt(2) :: Nil), JInt(1)) mustEqual(JArray(JInt(1) :: Nil))
  }
  "pop empty array" in {
    PopFieldEvalutor(JArray(Nil), JInt(1)) mustEqual(JArray(Nil))
  }
}