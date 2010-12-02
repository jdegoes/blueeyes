package blueeyes.persistence.mongo.mock

import org.spex.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvalutors._

class PullAllFieldEvalutorSpec  extends Specification{
  "pull elements" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo" pullAll (MongoPrimitiveInt(1), MongoPrimitiveInt(2))      
    PullAllFieldEvalutor(JArray(JInt(1) :: JInt(2) :: JInt(3) :: Nil), operation.filter) mustEqual(JArray(JInt(3) :: Nil))
  }
}