package blueeyes.persistence.mongo

import org.spex.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvalutors._

class AddToSetFieldEvalutorSpec  extends Specification{
  "create new Array for not existing field" in {
    AddToSetFieldEvalutor(JNothing, JObject(JField("$each", JArray(JString("foo") :: JString("bar") :: Nil)) :: Nil)) mustEqual(JArray(JString("foo") :: JString("bar") :: Nil))
  }
  "add new element to existing field" in {
    AddToSetFieldEvalutor(JArray(JInt(2) :: Nil), JObject(JField("$each", JArray(JInt(3) :: JInt(4) :: Nil)) :: Nil)) mustEqual(JArray(JInt(2) :: JInt(3) :: JInt(4) :: Nil))
  }
  "can add not array" in {
    AddToSetFieldEvalutor(JArray(JInt(2) :: Nil), JInt(3)) mustEqual(JArray(JInt(2) :: JInt(3) :: Nil))
  }
  "add array with existing element" in {
    AddToSetFieldEvalutor(JArray(JInt(2) :: Nil), JObject(JField("$each", JArray(JInt(3) :: JInt(2) :: Nil)) :: Nil)) mustEqual(JArray(JInt(2) :: JInt(3) :: Nil))
  }
  "does not add existing element" in {
    AddToSetFieldEvalutor(JArray(JInt(2) :: Nil), JInt(2)) mustEqual(JArray(JInt(2) :: Nil))
  }
  "cannot add to not Array field" in {
    AddToSetFieldEvalutor(JInt(2), JInt(3)) must throwA[MongoException]
  }
}