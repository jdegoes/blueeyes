package blueeyes.persistence.mongo.mock

import org.spex.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvalutors._
import blueeyes.json.JPathImplicits._
import blueeyes.persistence.mongo.MongoImplicits._

class PushFieldEvalutorSpec  extends Specification{
  "create new Array for not existing field" in {
    import blueeyes.persistence.mongo.MongoFilterImplicits._

    val operation = "foo" inc (MongoPrimitiveInt(2))
    PushFieldEvalutor(JNothing, operation.filter) mustEqual(JArray(JInt(2) :: Nil))
  }
  "add new element existing field" in {
    import blueeyes.persistence.mongo.MongoFilterImplicits._

    val operation = "foo" inc (MongoPrimitiveInt(3))
    PushFieldEvalutor(JArray(JInt(2) :: Nil), operation.filter) mustEqual(JArray(JInt(2) :: JInt(3) :: Nil))
  }
  "cannot push to not Array field" in {
    import blueeyes.persistence.mongo.MongoFilterImplicits._

    val operation = "foo" inc (MongoPrimitiveInt(3))
    PushFieldEvalutor(JInt(2), operation.filter) must throwA[MongoException]
  }
}