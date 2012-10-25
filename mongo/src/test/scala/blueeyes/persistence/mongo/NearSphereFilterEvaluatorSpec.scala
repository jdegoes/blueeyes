package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json._
import Evaluators._

class NearSphereFilterEvaluatorSpec extends Specification{
  private val near = JObject(List(JField("$nearSphere", JArray(List(JNum(1.0), JNum(1.0))))))
  private val nearDistance = JObject(List(JField("$nearSphere", JArray(List(JNum(-1.0), JNum(-2.0)))), JField("$maxDistance", JNum(1.0))))

  "NearFilterEvaluator" should{
    "return true when object has int array geo fields and maxDistance is not specified" in {
      NearSphereFilterEvaluator(JArray(JNum(10) :: JNum(11) :: Nil), near) must be_==(true)
    }
    "return true when object has double array geo fields and maxDistance is not specified" in {
      NearSphereFilterEvaluator(JArray(JNum(10.0) :: JNum(11.0) :: Nil), near) must be_==(true)
    }
    "return true when object has double and int array geo fields and maxDistance is not specified" in {
      NearSphereFilterEvaluator(JArray(JNum(10.0) :: JNum(11) :: Nil), near) must be_==(true)
    }
    "return true when object has 'int' JObject geo fields and maxDistance is not specified" in {
      NearSphereFilterEvaluator(JObject(JField("x", JNum(10)) :: JField("y", JNum(10)) :: Nil), near) must be_==(true)
    }
    "return true when object has double JObject geo fields and maxDistance is not specified" in {
      NearSphereFilterEvaluator(JObject(JField("x", JNum(10.0)) :: JField("y",  JNum(11.0)) :: Nil), near) must be_==(true)
    }
    "return true when object has int array geo fields and maxDistance is specified" in {
      NearSphereFilterEvaluator(JArray(JNum(1) :: JNum(2) :: Nil), nearDistance) must be_==(true)
    }
    "return true when object has double array geo fields and maxDistance is specified" in {
      NearSphereFilterEvaluator(JArray(JNum(1.0) :: JNum(2.0) :: Nil), nearDistance) must be_==(true)
    }
    "return true when object has 'int' JObject geo fields and maxDistance is specified" in {
      NearSphereFilterEvaluator(JObject(JField("x", JNum(1)) :: JField("y", JNum(2)) :: Nil), nearDistance) must be_==(true)
    }
    "return true when object has double JObject geo fields and maxDistance is specified" in {
      NearSphereFilterEvaluator(JObject(JField("x", JNum(1.0)) :: JField("y",  JNum(2.0)) :: Nil), nearDistance) must be_==(true)
    }
    "return false when object has double JObject geo fields and distance more then maxDistance" in {
      NearSphereFilterEvaluator(JObject(JField("x", JNum(90.0)) :: JField("y",  JNum(90.0)) :: Nil), nearDistance) must be_==(false)
    }
  }
}