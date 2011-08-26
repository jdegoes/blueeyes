package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class NearFilterEvaluatorSpec extends Specification{
  private val near = JObject(List(JField("$near", JArray(List(JDouble(1.0), JDouble(1.0))))))
  private val nearDistance = JObject(List(JField("$near", JArray(List(JDouble(-1.0), JDouble(-2.0)))), JField("$maxDistance", JDouble(2000.0))))

  "NearFilterEvaluator" should{
    "return true when object has int array geo fields and maxDistance is not specified" in {
      NearFilterEvaluator(JArray(JInt(10) :: JInt(11) :: Nil), near) must be (true)
    }
    "return true when object has double array geo fields and maxDistance is not specified" in {
      NearFilterEvaluator(JArray(JDouble(10.0) :: JDouble(11.0) :: Nil), near) must be (true)
    }
    "return true when object has double and int array geo fields and maxDistance is not specified" in {
      NearFilterEvaluator(JArray(JDouble(10.0) :: JInt(11) :: Nil), near) must be (true)
    }
    "return true when object has 'int' JObject geo fields and maxDistance is not specified" in {
      NearFilterEvaluator(JObject(JField("x", JInt(10)) :: JField("y", JInt(10)) :: Nil), near) must be (true)
    }
    "return true when object has double JObject geo fields and maxDistance is not specified" in {
      NearFilterEvaluator(JObject(JField("x", JDouble(10.0)) :: JField("y",  JDouble(11.0)) :: Nil), near) must be (true)
    }
    "return true when object has int array geo fields and maxDistance is specified" in {
      NearFilterEvaluator(JArray(JInt(10) :: JInt(20) :: Nil), nearDistance) must be (true)
    }
    "return true when object has double array geo fields and maxDistance is specified" in {
      NearFilterEvaluator(JArray(JDouble(10.0) :: JDouble(20.0) :: Nil), nearDistance) must be (true)
    }
    "return true when object has 'int' JObject geo fields and maxDistance is specified" in {
      NearFilterEvaluator(JObject(JField("x", JInt(10)) :: JField("y", JInt(20)) :: Nil), nearDistance) must be (true)
    }
    "return true when object has double JObject geo fields and maxDistance is specified" in {
      NearFilterEvaluator(JObject(JField("x", JDouble(10.0)) :: JField("y",  JDouble(20.0)) :: Nil), nearDistance) must be (true)
    }
    "return false when object has double JObject geo fields and distance more then maxDistance" in {
      NearFilterEvaluator(JObject(JField("x", JDouble(100.0)) :: JField("y",  JDouble(20.0)) :: Nil), nearDistance) must be (false)
    }
  }
}