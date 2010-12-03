package blueeyes.persistence.mongo.mock

import org.spex.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvaluators._

class IncFieldEvaluatorSpec extends Specification{
  "increare Integers" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo" inc (MongoPrimitiveInt(3))
    IncFieldEvaluator(JInt(1), operation.filter) mustEqual(JInt(4))
  }
  "increase Doubles" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo" inc (MongoPrimitiveDouble(2.2))
    IncFieldEvaluator(JDouble(1.1), operation.filter) mustEqual(JDouble(1.1 + 2.2))
  }
  "increase Integer and Double" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo" inc (MongoPrimitiveDouble(2.2))
    IncFieldEvaluator(JInt(1), operation.filter) mustEqual(JDouble(1 + 2.2))
  }
  "increase Double and Integer" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo" inc (MongoPrimitiveInt(1))
    IncFieldEvaluator(JDouble(2.2), operation.filter) mustEqual(JDouble(2.2 + 1))
  }
  "cannot increase not number" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo" inc (MongoPrimitiveInt(1))
    IncFieldEvaluator(JString("foo"), operation.filter) must throwA[MongoException]
  }
  "cannot be increases by not number" in {
    import blueeyes.persistence.mongo.MongoImplicits._

    val operation = "foo" inc (MongoPrimitiveString("foo"))
    IncFieldEvaluator(JInt(1), operation.filter) must throwA[MongoException]
  }
}