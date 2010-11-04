package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import MockMongoImplementation._

class TypeFieldFilterEvalutorSpec  extends Specification {
  "supports JArray tyoe" in {
    TypeFieldFilterEvalutor(JArray(JInt(2) :: JInt(3) :: Nil ), JInt(MongoFilterImplicits.MongoPrimitiveJArrayWitness.typeNumber)) must be (true)
  }
  "supports JString tyoe" in {
    TypeFieldFilterEvalutor(JString("foo"), JInt(MongoFilterImplicits.MongoPrimitiveJStringWitness.typeNumber)) must be (true)
  }
  "supports JDouble tyoe" in {
    TypeFieldFilterEvalutor(JDouble(2.2), JInt(MongoFilterImplicits.MongoPrimitiveJDoubleWitness.typeNumber)) must be (true)
  }
  "supports JObject tyoe" in {
    TypeFieldFilterEvalutor(JObject(JField("foo", JInt(2)) :: Nil ), JInt(MongoFilterImplicits.MongoPrimitiveJObjectWitness.typeNumber)) must be (true)
  }
  "supports JBool tyoe" in {
    TypeFieldFilterEvalutor(JBool(true), JInt(MongoFilterImplicits.MongoPrimitiveJBoolWitness.typeNumber)) must be (true)
  }
  "supports JNull tyoe" in {
    TypeFieldFilterEvalutor(JNull, JInt(MongoFilterImplicits.MongoPrimitiveJNullWitness.typeNumber)) must be (true)
  }
  "supports JInt tyoe" in {
    TypeFieldFilterEvalutor(JInt(1), JInt(MongoFilterImplicits.MongoPrimitiveJIntWitness.typeNumber)) must be (true)
  }
  "return false for wrong type" in {
    TypeFieldFilterEvalutor(JString("foo"), JInt(MongoFilterImplicits.MongoPrimitiveJIntWitness.typeNumber)) must be (false)
  }

}