package blueeyes.persistence.mongo

import org.spex.Specification
import MockMongoUpdateEvalutors._
import blueeyes.json.JsonAST.JString

class SetFieldEvalutorSpec extends Specification{
  "returns value as it is" in {
    SetFieldEvalutor(JString("foo"), JString("bar")) mustEqual(JString("bar"))  
  }
}