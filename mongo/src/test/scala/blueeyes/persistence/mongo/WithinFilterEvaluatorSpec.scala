package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo.Evaluators.WithinFilterEvaluator
import com.mongodb.MongoException

class WithinFilterEvaluatorSpec extends Specification {
  private val withinRadius       = JObject(JField("$within", JObject(JField("$center",  JArray(JArray(JDouble(10.0) :: JDouble(10.0) :: Nil) :: JDouble(500.0) :: Nil)) :: Nil)) :: Nil)
  private val withinBox          = JObject(JField("$within", JObject(JField("$box",     JArray(JArray(JDouble(-10.0) :: JDouble(-20.0) :: Nil) :: JArray(JDouble(20.0) :: JDouble(20.0) :: Nil) :: Nil)) :: Nil)) :: Nil)
  private val withinPolygon      = JObject(JField("$within", JObject(JField("$polygon", JArray(JArray(JDouble(-20.0) :: JDouble(30.0) :: Nil) :: JArray(JDouble(-20.0) :: JDouble(10.0) :: Nil) :: JArray(JDouble(-10.0) :: JDouble(10.0) :: Nil) :: JArray(JDouble(-10.0) :: JDouble(30.0) :: Nil) :: Nil)) :: Nil)) :: Nil)
  private val withinCenterSphere = JObject(JField("$within", JObject(JField("$centerSphere",  JArray(JArray(JDouble(10.0) :: JDouble(10.0) :: Nil) :: JDouble(0.1) :: Nil)) :: Nil)) :: Nil)

  "WithinFilterEvaluator.radius" should {
    "return true when object has int array geo fields and point is in the circle" in {
      WithinFilterEvaluator(JArray(JInt(15) :: JInt(15) :: Nil), withinRadius) must be_==(true)
    }
    "return false when object has int array geo fields and point is not in the circle" in {
      WithinFilterEvaluator(JArray(JInt(45) :: JInt(15) :: Nil), withinRadius) must be_==(false)
    }
    "return true when object has double JObject geo fields and point is in the circle" in {
      WithinFilterEvaluator(JObject(JField("x", JDouble(12.0)) :: JField("y",  JDouble(11.0)) :: Nil), withinRadius) must be_==(true)
    }
    "return false when object has double JObject geo fields and point is not in the circle" in {
      WithinFilterEvaluator(JObject(JField("x", JDouble(42.0)) :: JField("y",  JDouble(11.0)) :: Nil), withinRadius) must be_==(false)
    }
  }
  "WithinFilterEvaluator.centerSphere" should {
    "return true when object has int array geo fields and point is in the circle" in {
      WithinFilterEvaluator(JArray(JInt(11) :: JInt(11) :: Nil), withinCenterSphere) must be_==(true)
    }
    "return false when object has int array geo fields and point is not in the circle" in {
      WithinFilterEvaluator(JArray(JInt(45) :: JInt(15) :: Nil), withinCenterSphere) must be_==(false)
    }
    "return true when object has double JObject geo fields and point is in the circle" in {
      WithinFilterEvaluator(JObject(JField("x", JDouble(12.0)) :: JField("y",  JDouble(11.0)) :: Nil), withinCenterSphere) must be_==(true)
    }
    "return false when object has double JObject geo fields and point is not in the circle" in {
      WithinFilterEvaluator(JObject(JField("x", JDouble(42.0)) :: JField("y",  JDouble(11.0)) :: Nil), withinCenterSphere) must be_==(false)
    }
    "throws MongoExceptions if Radians is too big" in {
      WithinFilterEvaluator(JObject(JField("x", JDouble(42.0)) :: JField("y",  JDouble(11.0)) :: Nil), JObject(JField("$within", JObject(JField("$centerSphere",  JArray(JArray(JDouble(10.0) :: JDouble(10.0) :: Nil) :: JDouble(0.5) :: Nil)) :: Nil)) :: Nil)) must throwAn[MongoException]
    }
  }
  "WithinFilterEvaluator.box" should{
    "return true when object has int array geo fields and point is in the box" in {
      WithinFilterEvaluator(JArray(JInt(-5) :: JInt(-15) :: Nil), withinBox) must be_==(true)
    }
    "return false when object has int array geo fields and point is not in the box" in {
      WithinFilterEvaluator(JArray(JInt(-45) :: JInt(-15) :: Nil), withinBox) must be_==(false)
    }
    "return true when object has double JObject geo fields and point is in the box" in {
      WithinFilterEvaluator(JObject(JField("x", JDouble(12.0)) :: JField("y",  JDouble(11.0)) :: Nil), withinBox) must be_==(true)
    }
    "return false when object has double JObject geo fields and point is not in the box" in {
      WithinFilterEvaluator(JObject(JField("x", JDouble(42.0)) :: JField("y",  JDouble(11.0)) :: Nil), withinBox) must be_==(false)
    }
  }
  "WithinFilterEvaluator.polygon" should{
    "return true when object has int array geo fields and point is in the polygon" in {
      WithinFilterEvaluator(JArray(JInt(-10) :: JInt(20) :: Nil), withinPolygon) must be_==(true)
    }
    "return false when object has int array geo fields and point is not in the polygon" in {
      WithinFilterEvaluator(JArray(JInt(20) :: JInt(30) :: Nil), withinPolygon) must be_==(false)
    }
    "return true when object has double JObject geo fields and point is in the polygon" in {
      WithinFilterEvaluator(JObject(JField("x", JDouble(-10)) :: JField("y",  JDouble(20.0)) :: Nil), withinPolygon) must be_==(true)
    }
    "return false when object has double JObject geo fields and point is not in the polygon" in {
      WithinFilterEvaluator(JObject(JField("x", JDouble(20.0)) :: JField("y",  JDouble(30.0)) :: Nil), withinPolygon) must be_==(false)
    }
  }
}