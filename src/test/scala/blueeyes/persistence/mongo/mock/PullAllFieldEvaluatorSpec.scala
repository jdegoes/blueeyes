package blueeyes.persistence.mongo.mock

import org.spex.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvaluators._

class PullAllFieldEvaluatorSpec  extends Specification{
  "pull elements" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo" pullAll (MongoPrimitiveInt(1), MongoPrimitiveInt(2))      
    PullAllFieldEvaluator(JArray(JInt(1) :: JInt(2) :: JInt(3) :: Nil), operation.filter) mustEqual(JArray(JInt(3) :: Nil))
  }
}