package blueeyes.persistence.mongo.mock

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvaluators._
import blueeyes.persistence.mongo._

class IncFieldEvaluatorSpec extends Specification{
  "increare Integers" in {
    val operation = ("foo" inc (MongoPrimitiveInt(3))).asInstanceOf[MongoUpdateField]
    IncFieldEvaluator(JNum(1), operation.filter) mustEqual(JNum(4))
  }
  "increare JNothing" in {
    val operation = ("foo" inc (MongoPrimitiveInt(3))).asInstanceOf[MongoUpdateField]
    IncFieldEvaluator(JNothing, operation.filter) mustEqual(JNum(3))
  }
  "increase Doubles" in {
    val operation = ("foo" inc (MongoPrimitiveDouble(2.2))).asInstanceOf[MongoUpdateField]
    IncFieldEvaluator(JNum(1.1), operation.filter) mustEqual(JNum(3.3))
  }
  "increase Integer and Double" in {
    val operation = ("foo" inc (MongoPrimitiveDouble(2.2))).asInstanceOf[MongoUpdateField]
    IncFieldEvaluator(JNum(1), operation.filter) mustEqual(JNum(1 + 2.2))
  }
  "increase Double and Integer" in {
    val operation = ("foo" inc (MongoPrimitiveInt(1))).asInstanceOf[MongoUpdateField]
    IncFieldEvaluator(JNum(2.2), operation.filter) mustEqual(JNum(2.2 + 1))
  }
  "cannot increase not number" in {
    val operation = ("foo" inc (MongoPrimitiveInt(1))).asInstanceOf[MongoUpdateField]
    IncFieldEvaluator(JString("foo"), operation.filter) must throwA[MongoException]
  }
  "cannot be increases by not number" in {
    val operation = ("foo" inc (MongoPrimitiveString("foo"))).asInstanceOf[MongoUpdateField]
    IncFieldEvaluator(JNum(1), operation.filter) must throwA[MongoException]
  }
}