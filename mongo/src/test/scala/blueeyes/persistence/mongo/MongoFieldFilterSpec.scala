package blueeyes.persistence.mongo

import dsl._
import org.specs2.mutable.Specification
import blueeyes.json.JPathImplicits._
import blueeyes.json._
import MongoFilterOperators._
import blueeyes.json._

class MongoFieldFilterSpec extends Specification{
  "creates valid json for $regex operator" in{
    MongoFieldFilter("foo", $regex, JObject(List(JField("$regex", JString("bar")), JField("$options", JString("i"))))).filter mustEqual (JObject(JField("foo", JObject(List(JField("$regex", JString("bar")), JField("$options", JString("i"))))) :: Nil))
  }
  "creates valid json for $eq operator" in{
    MongoFieldFilter("foo", $eq, "bar").filter mustEqual (JObject(JField("foo", JString("bar")) :: Nil))
  }
  "creates valid json for $near operator" in{
    MongoFieldFilter("foo", $near, JObject(List(JField("$near", JArray(List(JNum(1), JNum(2))))))).filter mustEqual (JObject(JField("foo", JObject(List(JField("$near", JArray(List(JNum(1), JNum(2))))))) :: Nil))
  }
  "creates valid json for $near operator with $maxDistance" in{
    MongoFieldFilter("foo", $near, JObject(List(JField("$near", JArray(List(JNum(1), JNum(2)))), JField("$maxDistance", JNum(10))))).filter mustEqual (JObject(JField("foo", JObject(List(JField("$near", JArray(List(JNum(1), JNum(2)))), JField("$maxDistance", JNum(10))))) :: Nil))
  }
  "creates valid json for $nearSphere operator" in{
    MongoFieldFilter("foo", $nearSphere, JObject(List(JField("$nearSphere", JArray(List(JNum(1), JNum(2))))))).filter mustEqual (JObject(JField("foo", JObject(List(JField("$nearSphere", JArray(List(JNum(1), JNum(2))))))) :: Nil))
  }
  "creates valid json for $nearSphere operator with $maxDistance" in{
    MongoFieldFilter("foo", $nearSphere, JObject(List(JField("$nearSphere", JArray(List(JNum(1), JNum(2)))), JField("$maxDistance", JNum(10))))).filter mustEqual (JObject(JField("foo", JObject(List(JField("$nearSphere", JArray(List(JNum(1), JNum(2)))), JField("$maxDistance", JNum(10))))) :: Nil))
  }
  "creates valid json for $eq operator for empty path" in{
    MongoFieldFilter("", $eq, "bar").filter mustEqual (JString("bar"))
  }
  "creates valid json for $eq operator and complex path operator" in{
    MongoFieldFilter("author.name", $eq, "joe").filter mustEqual (JObject(JField("author.name", JString("joe")) :: Nil))
  }
  "creates valid json for $where operator" in{
    MongoFieldFilter(JPath.Identity, $where, "joe").filter mustEqual (JObject(JField("$where", JString("joe")) :: Nil))
  }
  "creates valid json for another operator then $eq" in{
    MongoFieldFilter("foo", $ne, "bar").filter mustEqual (JObject(JField("foo", JObject(JField("$ne", JString("bar")) :: Nil)) :: Nil))
  }
  "creates valid json for another operator then $eq and complex path operator" in{
    MongoFieldFilter("author.name", $ne, "joe").filter mustEqual (JObject(JField("author.name", JObject(JField("$ne", JString("joe")) :: Nil)) :: Nil))
  }

  "unary_! use negative operator" in{
    MongoFieldFilter("foo", $eq, "bar").unary_! mustEqual(MongoFieldFilter("foo", $ne, "bar"))
  }
}
