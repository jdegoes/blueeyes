package blueeyes.persistence.mongo

import org.spex.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvalutors._

class PushAllFieldEvalutorSpec  extends Specification{
  "create new Array for not existing field" in {
    PushAllFieldEvalutor(JNothing, JArray(JInt(2) :: Nil)) mustEqual(JArray(JInt(2) :: Nil))
  }
  "add new element existing field" in {
    PushAllFieldEvalutor(JArray(JInt(2) :: Nil), JArray(JInt(3) :: Nil)) mustEqual(JArray(JInt(2) :: JInt(3) :: Nil))
  }
  "can push not array" in {
    PushAllFieldEvalutor(JArray(JInt(2) :: Nil), JInt(3)) mustEqual(JArray(JInt(2) :: JInt(3) :: Nil))
  }
  "cannot push to not Array field" in {
    PushAllFieldEvalutor(JInt(2), JInt(3)) must throwA[MongoException]
  }
}