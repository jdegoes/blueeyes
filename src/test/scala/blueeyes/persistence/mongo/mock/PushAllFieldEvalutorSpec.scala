package blueeyes.persistence.mongo.mock

import org.spex.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvalutors._

class PushAllFieldEvalutorSpec  extends Specification{
  "create new Array for not existing field" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo" pushAll (MongoPrimitiveInt(2))
    PushAllFieldEvalutor(JNothing, operation.filter) mustEqual(JArray(JInt(2) :: Nil))
  }
  "add new element existing field" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo" pushAll (MongoPrimitiveInt(3))
    PushAllFieldEvalutor(JArray(JInt(2) :: Nil), operation.filter) mustEqual(JArray(JInt(2) :: JInt(3) :: Nil))
  }
}