package blueeyes.persistence.mongo

import org.spex.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvalutors._

class IncFieldEvalutorSpec extends Specification{
  "increare Integers" in {
    IncFieldEvalutor(JInt(1), JInt(2)) mustEqual(JInt(3))
  }
  "increase Doubles" in {
    IncFieldEvalutor(JDouble(1.1), JDouble(2.2)) mustEqual(JDouble(1.1 + 2.2))
  }
  "increase Integer and Double" in {
    IncFieldEvalutor(JInt(1), JDouble(2.2)) mustEqual(JDouble(1 + 2.2))
  }
  "increase Double and Integer" in {
    IncFieldEvalutor(JDouble(2.2), JInt(1)) mustEqual(JDouble(2.2 + 1))
  }
  "cannot increase not number" in {
    IncFieldEvalutor(JString("foo"), JInt(1)) must throwA[MongoException]
  }
  "cannot be increases by not number" in {
    IncFieldEvalutor(JInt(1), JString("foo")) must throwA[MongoException]
  }
}