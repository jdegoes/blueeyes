package blueeyes.persistence.mongo.mock

import org.specs.Specification
import MockMongoUpdateEvaluators._
import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo._

class UnsetFieldEvaluatorSpec extends Specification{
  "returns value as it is" in {
    val operation = ("foo".unset).asInstanceOf[MongoUpdateField]
    UnsetFieldEvaluator(JString("foo"), operation.filter) must be(JNothing)
  }
}