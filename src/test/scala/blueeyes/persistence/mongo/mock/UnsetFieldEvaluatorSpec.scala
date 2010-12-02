package blueeyes.persistence.mongo.mock

import org.spex.Specification
import MockMongoUpdateEvaluators._
import blueeyes.json.JsonAST._

class UnsetFieldEvaluatorSpec extends Specification{
  "returns value as it is" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo".unset
    UnsetFieldEvaluator(JString("foo"), operation.filter) must be(JNothing)
  }
}