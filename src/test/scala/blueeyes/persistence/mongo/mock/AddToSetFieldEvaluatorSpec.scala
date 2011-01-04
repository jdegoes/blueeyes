package blueeyes.persistence.mongo.mock

import org.spex.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import MockMongoUpdateEvaluators._
import blueeyes.persistence.mongo._

class AddToSetFieldEvaluatorSpec  extends Specification{
  "create new Array for not existing field" in {
    val operation = "foo" addToSet (MongoPrimitiveString("foo"), MongoPrimitiveString("bar"))
    AddToSetFieldEvaluator(JNothing, operation.filter) mustEqual(JArray(JString("foo") :: JString("bar") :: Nil))
  }
  "add new element to existing field" in {
    val operation = "foo" addToSet (MongoPrimitiveInt(3), MongoPrimitiveInt(4))
    AddToSetFieldEvaluator(JArray(JInt(2) :: Nil), operation.filter) mustEqual(JArray(JInt(2) :: JInt(3) :: JInt(4) :: Nil))
  }
  "can add not array" in {
    val operation = "foo" addToSet (MongoPrimitiveInt(3))
    AddToSetFieldEvaluator(JArray(JInt(2) :: Nil), operation.filter) mustEqual(JArray(JInt(2) :: JInt(3) :: Nil))
  }
  "add array with existing element" in {
    val operation = "foo" addToSet (MongoPrimitiveInt(3), MongoPrimitiveInt(2))
    AddToSetFieldEvaluator(JArray(JInt(2) :: Nil), operation.filter) mustEqual(JArray(JInt(2) :: JInt(3) :: Nil))
  }
  "does not add existing element" in {
    val operation = "foo" addToSet (MongoPrimitiveInt(2))
    AddToSetFieldEvaluator(JArray(JInt(2) :: Nil), operation.filter) mustEqual(JArray(JInt(2) :: Nil))
  }
  "cannot add to not Array field" in {
    val operation = "foo" addToSet (MongoPrimitiveInt(3))
    AddToSetFieldEvaluator(JInt(2), operation.filter) must throwA[MongoException]
  }
}