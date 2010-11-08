package blueeyes.persistence.mongo

import org.spex.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvalutors._
import blueeyes.json.JPathImplicits._
import MongoImplicits._

class PullAllFieldEvalutorSpec  extends Specification{
  "pull elements" in {
    import MongoFilterImplicits._

    val operation = "foo" pullAll (MongoPrimitiveInt(1), MongoPrimitiveInt(2))      
    PullAllFieldEvalutor(JArray(JInt(1) :: JInt(2) :: JInt(3) :: Nil), operation.filter) mustEqual(JArray(JInt(3) :: Nil))
  }
}