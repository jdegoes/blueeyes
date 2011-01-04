package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class TypeFieldFilterEvaluatorSpec  extends Specification {
  "supports JArray tyoe" in {
    TypeFieldFilterEvaluator(JArray(JInt(2) :: JInt(3) :: Nil ), JInt(MongoPrimitiveJArrayWitness.typeNumber)) must be (true)
  }
  "supports JString tyoe" in {
    TypeFieldFilterEvaluator(JString("foo"), JInt(MongoPrimitiveJStringWitness.typeNumber)) must be (true)
  }
  "supports JDouble tyoe" in {
    TypeFieldFilterEvaluator(JDouble(2.2), JInt(MongoPrimitiveJDoubleWitness.typeNumber)) must be (true)
  }
  "supports JObject tyoe" in {
    TypeFieldFilterEvaluator(JObject(JField("foo", JInt(2)) :: Nil ), JInt(MongoPrimitiveJObjectWitness.typeNumber)) must be (true)
  }
  "supports JBool tyoe" in {
    TypeFieldFilterEvaluator(JBool(true), JInt(MongoPrimitiveJBoolWitness.typeNumber)) must be (true)
  }
  "supports JNull tyoe" in {
    TypeFieldFilterEvaluator(JNull, JInt(MongoPrimitiveJNullWitness.typeNumber)) must be (true)
  }
  "supports JInt tyoe" in {
    TypeFieldFilterEvaluator(JInt(1), JInt(MongoPrimitiveJIntWitness.typeNumber)) must be (true)
  }
  "return false for wrong type" in {
    TypeFieldFilterEvaluator(JString("foo"), JInt(MongoPrimitiveJIntWitness.typeNumber)) must be (false)
  }

}