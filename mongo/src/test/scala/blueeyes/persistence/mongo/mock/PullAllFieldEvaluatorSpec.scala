package blueeyes.persistence.mongo.mock

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvaluators._
import blueeyes.persistence.mongo._

class PullAllFieldEvaluatorSpec  extends Specification{
  "pull elements" in {
    val operation = ("foo" pullAll (MongoPrimitiveInt(1), MongoPrimitiveInt(2))).asInstanceOf[MongoUpdateField]
    PullAllFieldEvaluator(JArray(JNum(1) :: JNum(2) :: JNum(3) :: Nil), operation.filter) mustEqual(JArray(JNum(3) :: Nil))
  }
}