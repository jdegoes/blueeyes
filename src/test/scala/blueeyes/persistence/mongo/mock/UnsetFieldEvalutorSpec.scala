package blueeyes.persistence.mongo.mock

import org.spex.Specification
import MockMongoUpdateEvalutors._
import blueeyes.json.JsonAST._

class UnsetFieldEvalutorSpec extends Specification{
  "returns value as it is" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo".unset
    UnsetFieldEvalutor(JString("foo"), operation.filter) must be(JNothing)
  }
}