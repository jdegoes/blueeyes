package blueeyes.persistence.mongo.mock

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._


class GeoFieldOrderingSpec extends Specification{
  private val ordering = GeoFieldOrdering("location", 1.0, 1.0)
  "GeoFieldOrdering" should{
    "sort objects with geo int array fields" in {
      ordering.compare(JObject(List(JField("location", JArray(List(JInt(2), JInt(3)))))), JObject(List(JField("location", JArray(List(JInt(5), JInt(5))))))) must be_==(-1)
    }
    "sort objects with geo int and double array fields" in {
      ordering.compare(JObject(List(JField("location", JArray(List(JInt(2), JDouble(3)))))), JObject(List(JField("location", JArray(List(JDouble(5), JInt(5))))))) must be_==(-1)
    }
    "sort objects with geo Double array fields" in {
      ordering.compare(JObject(List(JField("location", JArray(List(JDouble(5.0), JDouble(5.0)))))), JObject(List(JField("location", JArray(List(JDouble(2.0), JDouble(3.0))))))) must be_==(1)
    }
    "sort objects with geo int jobject fields" in {
      ordering.compare(JObject(List(JField("location", JObject(JField("x", JInt(2)) :: JField("y", JInt(3)) :: Nil)))), JObject(List(JField("location", JObject(JField("x", JInt(5)) :: JField("y", JInt(5)) :: Nil))))) must be_==(-1)
    }
    "sort objects with geo int and double jobject fields" in {
      ordering.compare(JObject(List(JField("location", JObject(JField("x", JDouble(2.0)) :: JField("y", JInt(3)) :: Nil)))), JObject(List(JField("location", JObject(JField("x", JInt(5)) :: JField("y", JDouble(5.0)) :: Nil))))) must be_==(-1)
    }
    "sort objects with different geo fields" in {
      ordering.compare(JObject(List(JField("location", JArray(List(JDouble(5), JInt(5)))))), JObject(List(JField("location", JObject(JField("x", JDouble(2.0)) :: JField("y", JInt(3)) :: Nil))))) must be_==(1)
    }
    "sort objects with geo double jobject fields" in {
      ordering.compare(JObject(List(JField("location", JObject(JField("x", JDouble(2.0)) :: JField("y", JDouble(3.0)) :: Nil)))), JObject(List(JField("location", JObject(JField("x", JDouble(5.0)) :: JField("y", JDouble(5.0)) :: Nil))))) must be_==(-1)
    }
  }
}