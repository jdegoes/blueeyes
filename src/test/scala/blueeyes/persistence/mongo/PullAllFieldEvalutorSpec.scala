package blueeyes.persistence.mongo

import org.spex.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvalutors._

class PullAllFieldEvalutorSpec  extends Specification{
  "pull elements" in {
    PullAllFieldEvalutor(JArray(JInt(1) :: JInt(2) :: JInt(3) :: Nil), JArray(JInt(1) :: JInt(2) :: Nil)) mustEqual(JArray(JInt(3) :: Nil))
  }
}