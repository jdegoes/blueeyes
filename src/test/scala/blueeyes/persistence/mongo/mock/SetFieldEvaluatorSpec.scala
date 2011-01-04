package blueeyes.persistence.mongo.mock

import org.spex.Specification
import MockMongoUpdateEvaluators._
import blueeyes.json.JsonAST.JString
import blueeyes.persistence.mongo._

class SetFieldEvaluatorSpec extends Specification{
  "returns value as it is" in {
    val operation = "foo" set (MongoPrimitiveString("bar"))    
    SetFieldEvaluator(JString("foo"), operation.filter) mustEqual(JString("bar"))
  }
}