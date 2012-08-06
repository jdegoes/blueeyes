package blueeyes.persistence.mongo.mock

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvaluators._
import blueeyes.persistence.mongo._

class AddToSetFieldEvaluatorSpec  extends Specification{
  "create new Array for not existing field" in {
    val operation = ("foo" addToSet (MongoPrimitiveString("foo"), MongoPrimitiveString("bar"))).asInstanceOf[MongoUpdateField]
    AddToSetFieldEvaluator(JNothing, operation.filter) mustEqual(JArray(JString("foo") :: JString("bar") :: Nil))
  }
  "create new Array with duplicating elements for not existing field" in {
    val operation = ("foo" addToSet (MongoPrimitiveString("foo"), MongoPrimitiveString("foo"))).asInstanceOf[MongoUpdateField]
    AddToSetFieldEvaluator(JNothing, operation.filter) mustEqual(JArray(JString("foo") :: JString("foo") :: Nil))
  }
  "add new element to existing field" in {
    val operation = ("foo" addToSet (MongoPrimitiveInt(3), MongoPrimitiveInt(4))).asInstanceOf[MongoUpdateField]
    AddToSetFieldEvaluator(JArray(JNum(2) :: Nil), operation.filter) mustEqual(JArray(JNum(2) :: JNum(3) :: JNum(4) :: Nil))
  }
  "can add not array" in {
    val operation = ("foo" addToSet (MongoPrimitiveInt(3))).asInstanceOf[MongoUpdateField]
    AddToSetFieldEvaluator(JArray(JNum(2) :: Nil), operation.filter) mustEqual(JArray(JNum(2) :: JNum(3) :: Nil))
  }
  "add array with existing element" in {
    val operation = ("foo" addToSet (MongoPrimitiveInt(3), MongoPrimitiveInt(2))).asInstanceOf[MongoUpdateField]
    AddToSetFieldEvaluator(JArray(JNum(2) :: Nil), operation.filter) mustEqual(JArray(JNum(2) :: JNum(3) :: Nil))
  }
  "does not add existing element" in {
    val operation = ("foo" addToSet (MongoPrimitiveInt(2))).asInstanceOf[MongoUpdateField]
    AddToSetFieldEvaluator(JArray(JNum(2) :: Nil), operation.filter) mustEqual(JArray(JNum(2) :: Nil))
  }
  "cannot add to not Array field" in {
    val operation = ("foo" addToSet (MongoPrimitiveInt(3))).asInstanceOf[MongoUpdateField]
    AddToSetFieldEvaluator(JNum(2), operation.filter) must throwA[MongoException]
  }
}