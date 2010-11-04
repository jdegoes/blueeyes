package blueeyes.persistence.mongo

import org.spex.Specification
import MockMongoUpdateEvalutors._
import blueeyes.json.JsonAST._

class UnsetFieldEvalutorSpec extends Specification{
  "returns value as it is" in {
    UnsetFieldEvalutor(JString("foo"), JString("bar")) must be(JNothing)
  }
}