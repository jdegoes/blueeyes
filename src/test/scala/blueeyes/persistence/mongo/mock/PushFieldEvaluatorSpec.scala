package blueeyes.persistence.mongo.mock

import org.spex.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvaluators._

class PushFieldEvaluatorSpec  extends Specification{
  "create new Array for not existing field" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo" inc (MongoPrimitiveInt(2))
    PushFieldEvaluator(JNothing, operation.filter) mustEqual(JArray(JInt(2) :: Nil))
  }
  "add new element existing field" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo" inc (MongoPrimitiveInt(3))
    PushFieldEvaluator(JArray(JInt(2) :: Nil), operation.filter) mustEqual(JArray(JInt(2) :: JInt(3) :: Nil))
  }
  "cannot push to not Array field" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo" inc (MongoPrimitiveInt(3))
    PushFieldEvaluator(JInt(2), operation.filter) must throwA[MongoException]
  }
}