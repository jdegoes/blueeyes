package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class TypeFieldFilterEvaluatorSpec  extends Specification {
  "supports JArray tyoe" in {
    TypeFieldFilterEvaluator(JArray(JInt(2) :: JInt(3) :: Nil ), JInt(MongoFilterImplicits.MongoPrimitiveJArrayWitness.typeNumber)) must be (true)
  }
  "supports JString tyoe" in {
    TypeFieldFilterEvaluator(JString("foo"), JInt(MongoFilterImplicits.MongoPrimitiveJStringWitness.typeNumber)) must be (true)
  }
  "supports JDouble tyoe" in {
    TypeFieldFilterEvaluator(JDouble(2.2), JInt(MongoFilterImplicits.MongoPrimitiveJDoubleWitness.typeNumber)) must be (true)
  }
  "supports JObject tyoe" in {
    TypeFieldFilterEvaluator(JObject(JField("foo", JInt(2)) :: Nil ), JInt(MongoFilterImplicits.MongoPrimitiveJObjectWitness.typeNumber)) must be (true)
  }
  "supports JBool tyoe" in {
    TypeFieldFilterEvaluator(JBool(true), JInt(MongoFilterImplicits.MongoPrimitiveJBoolWitness.typeNumber)) must be (true)
  }
  "supports JNull tyoe" in {
    TypeFieldFilterEvaluator(JNull, JInt(MongoFilterImplicits.MongoPrimitiveJNullWitness.typeNumber)) must be (true)
  }
  "supports JInt tyoe" in {
    TypeFieldFilterEvaluator(JInt(1), JInt(MongoFilterImplicits.MongoPrimitiveJIntWitness.typeNumber)) must be (true)
  }
  "return false for wrong type" in {
    TypeFieldFilterEvaluator(JString("foo"), JInt(MongoFilterImplicits.MongoPrimitiveJIntWitness.typeNumber)) must be (false)
  }

}