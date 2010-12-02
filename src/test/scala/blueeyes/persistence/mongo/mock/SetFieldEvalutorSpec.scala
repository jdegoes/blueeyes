package blueeyes.persistence.mongo.mock

import org.spex.Specification
import MockMongoUpdateEvalutors._
import blueeyes.json.JsonAST.JString

class SetFieldEvalutorSpec extends Specification{
  "returns value as it is" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo" set (MongoPrimitiveString("bar"))    
    SetFieldEvalutor(JString("foo"), operation.filter) mustEqual(JString("bar"))  
  }
}