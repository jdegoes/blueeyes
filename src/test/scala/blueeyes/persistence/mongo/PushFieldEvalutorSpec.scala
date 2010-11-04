package blueeyes.persistence.mongo

import org.spex.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvalutors._

class PushFieldEvalutorSpec  extends Specification{
  "create new Array for not existing field" in {
    PushFieldEvalutor(JNothing, JInt(2)) mustEqual(JArray(JInt(2) :: Nil))
  }
  "add new element existing field" in {
    PushFieldEvalutor(JArray(JInt(2) :: Nil), JInt(3)) mustEqual(JArray(JInt(2) :: JInt(3) :: Nil))
  }
  "cannot push to not Array field" in {
    PushFieldEvalutor(JInt(2), JInt(3)) must throwA[MongoException]
  }
}