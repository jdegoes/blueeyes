package blueeyes.persistence.mongo

import org.spex.Specification
import MockMongoUpdateEvalutors._
import blueeyes.json.JsonAST.JString
import blueeyes.json.JPathImplicits._
import MongoImplicits._

class SetFieldEvalutorSpec extends Specification{
  "returns value as it is" in {
    import MongoFilterImplicits._

    val operation = "foo" set (MongoPrimitiveString("bar"))    
    SetFieldEvalutor(JString("foo"), operation.filter) mustEqual(JString("bar"))  
  }
}