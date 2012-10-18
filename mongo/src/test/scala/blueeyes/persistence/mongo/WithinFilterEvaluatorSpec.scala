package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json._
import blueeyes.persistence.mongo.Evaluators.WithinFilterEvaluator
import com.mongodb.MongoException

class WithinFilterEvaluatorSpec extends Specification {
  private val withinRadius       = JObject(JField("$within", JObject(JField("$center",  JArray(JArray(JNum(10.0) :: JNum(10.0) :: Nil) :: JNum(500.0) :: Nil)) :: Nil)) :: Nil)
  private val withinBox          = JObject(JField("$within", JObject(JField("$box",     JArray(JArray(JNum(-10.0) :: JNum(-20.0) :: Nil) :: JArray(JNum(20.0) :: JNum(20.0) :: Nil) :: Nil)) :: Nil)) :: Nil)
  private val withinPolygon      = JObject(JField("$within", JObject(JField("$polygon", JArray(JArray(JNum(-20.0) :: JNum(30.0) :: Nil) :: JArray(JNum(-20.0) :: JNum(10.0) :: Nil) :: JArray(JNum(-10.0) :: JNum(10.0) :: Nil) :: JArray(JNum(-10.0) :: JNum(30.0) :: Nil) :: Nil)) :: Nil)) :: Nil)
  private val withinCenterSphere = JObject(JField("$within", JObject(JField("$centerSphere",  JArray(JArray(JNum(10.0) :: JNum(10.0) :: Nil) :: JNum(0.1) :: Nil)) :: Nil)) :: Nil)

  "WithinFilterEvaluator.radius" should {
    "return true when object has int array geo fields and point is in the circle" in {
      WithinFilterEvaluator(JArray(JNum(15) :: JNum(15) :: Nil), withinRadius) must be_==(true)
    }
    "return false when object has int array geo fields and point is not in the circle" in {
      WithinFilterEvaluator(JArray(JNum(45) :: JNum(15) :: Nil), withinRadius) must be_==(false)
    }
    "return true when object has double JObject geo fields and point is in the circle" in {
      WithinFilterEvaluator(JObject(JField("x", JNum(12.0)) :: JField("y",  JNum(11.0)) :: Nil), withinRadius) must be_==(true)
    }
    "return false when object has double JObject geo fields and point is not in the circle" in {
      WithinFilterEvaluator(JObject(JField("x", JNum(42.0)) :: JField("y",  JNum(11.0)) :: Nil), withinRadius) must be_==(false)
    }
  }
  "WithinFilterEvaluator.centerSphere" should {
    "return true when object has int array geo fields and point is in the circle" in {
      WithinFilterEvaluator(JArray(JNum(11) :: JNum(11) :: Nil), withinCenterSphere) must be_==(true)
    }
    "return false when object has int array geo fields and point is not in the circle" in {
      WithinFilterEvaluator(JArray(JNum(45) :: JNum(15) :: Nil), withinCenterSphere) must be_==(false)
    }
    "return true when object has double JObject geo fields and point is in the circle" in {
      WithinFilterEvaluator(JObject(JField("x", JNum(12.0)) :: JField("y",  JNum(11.0)) :: Nil), withinCenterSphere) must be_==(true)
    }
    "return false when object has double JObject geo fields and point is not in the circle" in {
      WithinFilterEvaluator(JObject(JField("x", JNum(42.0)) :: JField("y",  JNum(11.0)) :: Nil), withinCenterSphere) must be_==(false)
    }
    "throws MongoExceptions if Radians is too big" in {
      WithinFilterEvaluator(JObject(JField("x", JNum(42.0)) :: JField("y",  JNum(11.0)) :: Nil), JObject(JField("$within", JObject(JField("$centerSphere",  JArray(JArray(JNum(10.0) :: JNum(10.0) :: Nil) :: JNum(0.5) :: Nil)) :: Nil)) :: Nil)) must throwAn[MongoException]
    }
  }
  "WithinFilterEvaluator.box" should{
    "return true when object has int array geo fields and point is in the box" in {
      WithinFilterEvaluator(JArray(JNum(-5) :: JNum(-15) :: Nil), withinBox) must be_==(true)
    }
    "return false when object has int array geo fields and point is not in the box" in {
      WithinFilterEvaluator(JArray(JNum(-45) :: JNum(-15) :: Nil), withinBox) must be_==(false)
    }
    "return true when object has double JObject geo fields and point is in the box" in {
      WithinFilterEvaluator(JObject(JField("x", JNum(12.0)) :: JField("y",  JNum(11.0)) :: Nil), withinBox) must be_==(true)
    }
    "return false when object has double JObject geo fields and point is not in the box" in {
      WithinFilterEvaluator(JObject(JField("x", JNum(42.0)) :: JField("y",  JNum(11.0)) :: Nil), withinBox) must be_==(false)
    }
  }
  "WithinFilterEvaluator.polygon" should{
    "return true when object has int array geo fields and point is in the polygon" in {
      WithinFilterEvaluator(JArray(JNum(-10) :: JNum(20) :: Nil), withinPolygon) must be_==(true)
    }
    "return false when object has int array geo fields and point is not in the polygon" in {
      WithinFilterEvaluator(JArray(JNum(20) :: JNum(30) :: Nil), withinPolygon) must be_==(false)
    }
    "return true when object has double JObject geo fields and point is in the polygon" in {
      WithinFilterEvaluator(JObject(JField("x", JNum(-10)) :: JField("y",  JNum(20.0)) :: Nil), withinPolygon) must be_==(true)
    }
    "return false when object has double JObject geo fields and point is not in the polygon" in {
      WithinFilterEvaluator(JObject(JField("x", JNum(20.0)) :: JField("y",  JNum(30.0)) :: Nil), withinPolygon) must be_==(false)
    }
  }
}