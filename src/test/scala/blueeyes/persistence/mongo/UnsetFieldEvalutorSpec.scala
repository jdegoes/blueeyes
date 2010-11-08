package blueeyes.persistence.mongo

import org.spex.Specification
import MockMongoUpdateEvalutors._
import blueeyes.json.JsonAST._
import blueeyes.json.JPathImplicits._
import MongoImplicits._

class UnsetFieldEvalutorSpec extends Specification{
  "returns value as it is" in {
    import MongoFilterImplicits._

    val operation = "foo".unset
    UnsetFieldEvalutor(JString("foo"), operation.filter) must be(JNothing)
  }
}