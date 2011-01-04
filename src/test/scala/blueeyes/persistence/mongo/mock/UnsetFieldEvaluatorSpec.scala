package blueeyes.persistence.mongo.mock

import org.spex.Specification
import MockMongoUpdateEvaluators._
import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo._

class UnsetFieldEvaluatorSpec extends Specification{
  "returns value as it is" in {
    val operation = "foo".unset
    UnsetFieldEvaluator(JString("foo"), operation.filter) must be(JNothing)
  }
}