package blueeyes.persistence.mongo.mock

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvaluators._
import blueeyes.persistence.mongo._

class PushFieldEvaluatorSpec  extends Specification{
  "create new Array for not existing field" in {
    val operation = ("foo" inc (MongoPrimitiveInt(2))).asInstanceOf[MongoUpdateField]
    PushFieldEvaluator(JNothing, operation.filter) mustEqual(JArray(JNum(2) :: Nil))
  }
  "add new element existing field" in {
    val operation = ("foo" inc (MongoPrimitiveInt(3))).asInstanceOf[MongoUpdateField]
    PushFieldEvaluator(JArray(JNum(2) :: Nil), operation.filter) mustEqual(JArray(JNum(2) :: JNum(3) :: Nil))
  }
  "cannot push to not Array field" in {
    val operation = ("foo" inc (MongoPrimitiveInt(3))).asInstanceOf[MongoUpdateField]
    PushFieldEvaluator(JNum(2), operation.filter) must throwA[MongoException]
  }
}