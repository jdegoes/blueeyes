package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class TypeFieldFilterEvaluatorSpec  extends Specification {
  "supports JArray tyoe" in {
    TypeFieldFilterEvaluator(JArray(JNum(2) :: JNum(3) :: Nil ), JNum(MongoPrimitiveJArrayWitness.typeNumber)) must be_==(true)
  }
  "supports JString tyoe" in {
    TypeFieldFilterEvaluator(JString("foo"), JNum(MongoPrimitiveJStringWitness.typeNumber)) must be_==(true)
  }
  "supports JNum tyoe" in {
    TypeFieldFilterEvaluator(JNum(2.2), JNum(MongoPrimitiveJNumWitness.typeNumber)) must be_==(true)
  }
  "supports JObject tyoe" in {
    TypeFieldFilterEvaluator(JObject(JField("foo", JNum(2)) :: Nil ), JNum(MongoPrimitiveJObjectWitness.typeNumber)) must be_==(true)
  }
  "supports JBool tyoe" in {
    TypeFieldFilterEvaluator(JBool(true), JNum(MongoPrimitiveJBoolWitness.typeNumber)) must be_==(true)
  }
  "supports JNull tyoe" in {
    TypeFieldFilterEvaluator(JNull, JNum(MongoPrimitiveJNullWitness.typeNumber)) must be_==(true)
  }
  "supports JNum tyoe" in {
    TypeFieldFilterEvaluator(JNum(1), JNum(MongoPrimitiveJNumWitness.typeNumber)) must be_==(true)
  }
  "return false for wrong type" in {
    TypeFieldFilterEvaluator(JString("foo"), JNum(MongoPrimitiveJNumWitness.typeNumber)) must be_==(false)
  }

}