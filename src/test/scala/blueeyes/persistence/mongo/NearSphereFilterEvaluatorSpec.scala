package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class NearSphereFilterEvaluatorSpec extends Specification{
  private val near = JObject(List(JField("$nearSphere", JArray(List(JDouble(1.0), JDouble(1.0))))))
  private val nearDistance = JObject(List(JField("$nearSphere", JArray(List(JDouble(-1.0), JDouble(-2.0)))), JField("$maxDistance", JDouble(1.0))))

  "NearFilterEvaluator" should{
    "return true when object has int array geo fields and maxDistance is not specified" in {
      NearSphereFilterEvaluator(JArray(JInt(10) :: JInt(11) :: Nil), near) must be_==(true)
    }
    "return true when object has double array geo fields and maxDistance is not specified" in {
      NearSphereFilterEvaluator(JArray(JDouble(10.0) :: JDouble(11.0) :: Nil), near) must be_==(true)
    }
    "return true when object has double and int array geo fields and maxDistance is not specified" in {
      NearSphereFilterEvaluator(JArray(JDouble(10.0) :: JInt(11) :: Nil), near) must be_==(true)
    }
    "return true when object has 'int' JObject geo fields and maxDistance is not specified" in {
      NearSphereFilterEvaluator(JObject(JField("x", JInt(10)) :: JField("y", JInt(10)) :: Nil), near) must be_==(true)
    }
    "return true when object has double JObject geo fields and maxDistance is not specified" in {
      NearSphereFilterEvaluator(JObject(JField("x", JDouble(10.0)) :: JField("y",  JDouble(11.0)) :: Nil), near) must be_==(true)
    }
    "return true when object has int array geo fields and maxDistance is specified" in {
      NearSphereFilterEvaluator(JArray(JInt(1) :: JInt(2) :: Nil), nearDistance) must be_==(true)
    }
    "return true when object has double array geo fields and maxDistance is specified" in {
      NearSphereFilterEvaluator(JArray(JDouble(1.0) :: JDouble(2.0) :: Nil), nearDistance) must be_==(true)
    }
    "return true when object has 'int' JObject geo fields and maxDistance is specified" in {
      NearSphereFilterEvaluator(JObject(JField("x", JInt(1)) :: JField("y", JInt(2)) :: Nil), nearDistance) must be_==(true)
    }
    "return true when object has double JObject geo fields and maxDistance is specified" in {
      NearSphereFilterEvaluator(JObject(JField("x", JDouble(1.0)) :: JField("y",  JDouble(2.0)) :: Nil), nearDistance) must be_==(true)
    }
    "return false when object has double JObject geo fields and distance more then maxDistance" in {
      NearSphereFilterEvaluator(JObject(JField("x", JDouble(90.0)) :: JField("y",  JDouble(90.0)) :: Nil), nearDistance) must be_==(false)
    }
  }
}