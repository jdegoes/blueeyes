package blueeyes.persistence.mongo.mock

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import MockMongoUpdateEvaluators._
import blueeyes.persistence.mongo._

class PopFieldEvaluatorSpec  extends Specification{
  "pop first element" in {
    val operation = ("foo".popFirst).asInstanceOf[MongoUpdateField]
    PopFieldEvaluator(JArray(JNum(1) :: JNum(2) :: Nil), operation.filter) mustEqual(JArray(JNum(2) :: Nil))
  }
  "pop last element" in {
    val operation = ("foo".popLast).asInstanceOf[MongoUpdateField]
    PopFieldEvaluator(JArray(JNum(1) :: JNum(2) :: Nil), operation.filter) mustEqual(JArray(JNum(1) :: Nil))
  }
  "pop empty array" in {
    val operation = ("foo".popLast).asInstanceOf[MongoUpdateField]
    PopFieldEvaluator(JArray(Nil), operation.filter) mustEqual(JArray(Nil))
  }
}