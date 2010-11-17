package blueeyes.persistence.mongo.mock

import org.spex.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvalutors._
import blueeyes.json.JPathImplicits._
import blueeyes.persistence.mongo.MongoImplicits._

class IncFieldEvalutorSpec extends Specification{
  "increare Integers" in {
    import blueeyes.persistence.mongo.MongoFilterImplicits._

    val operation = "foo" inc (MongoPrimitiveInt(3))
    IncFieldEvalutor(JInt(1), operation.filter) mustEqual(JInt(4))
  }
  "increase Doubles" in {
    import blueeyes.persistence.mongo.MongoFilterImplicits._

    val operation = "foo" inc (MongoPrimitiveDouble(2.2))
    IncFieldEvalutor(JDouble(1.1), operation.filter) mustEqual(JDouble(1.1 + 2.2))
  }
  "increase Integer and Double" in {
    import blueeyes.persistence.mongo.MongoFilterImplicits._

    val operation = "foo" inc (MongoPrimitiveDouble(2.2))
    IncFieldEvalutor(JInt(1), operation.filter) mustEqual(JDouble(1 + 2.2))
  }
  "increase Double and Integer" in {
    import blueeyes.persistence.mongo.MongoFilterImplicits._

    val operation = "foo" inc (MongoPrimitiveInt(1))
    IncFieldEvalutor(JDouble(2.2), operation.filter) mustEqual(JDouble(2.2 + 1))
  }
  "cannot increase not number" in {
    import blueeyes.persistence.mongo.MongoFilterImplicits._

    val operation = "foo" inc (MongoPrimitiveInt(1))
    IncFieldEvalutor(JString("foo"), operation.filter) must throwA[MongoException]
  }
  "cannot be increases by not number" in {
    import blueeyes.persistence.mongo.MongoFilterImplicits._

    val operation = "foo" inc (MongoPrimitiveString("foo"))
    IncFieldEvalutor(JInt(1), operation.filter) must throwA[MongoException]
  }
}