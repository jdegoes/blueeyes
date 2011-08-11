package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo.Evaluators.WithinFilterEvaluator

class WithinFilterEvaluatorSpec extends Specification {
  private val withinRadius = JObject(JField("$within", JObject(JField("$center",  JArray(JArray(JDouble(10.0) :: JDouble(10.0) :: Nil) :: JDouble(20.0) :: Nil)) :: Nil)) :: Nil)

  "WithinFilterEvaluator.radius" should{
    "return true when object has int array geo fields and point is in the circle" in {
      WithinFilterEvaluator(JArray(JInt(15) :: JInt(15) :: Nil), withinRadius) must be (true)
    }
    "return false when object has int array geo fields and point is not in the circle" in {
      WithinFilterEvaluator(JArray(JInt(45) :: JInt(15) :: Nil), withinRadius) must be (false)
    }
    "return true when object has double JObject geo fields and point is in the circle" in {
      WithinFilterEvaluator(JObject(JField("x", JDouble(12.0)) :: JField("y",  JDouble(11.0)) :: Nil), withinRadius) must be (true)
    }
    "return false when object has double JObject geo fields and point is not in the circle" in {
      WithinFilterEvaluator(JObject(JField("x", JDouble(42.0)) :: JField("y",  JDouble(11.0)) :: Nil), withinRadius) must be (false)
    }
  }
}