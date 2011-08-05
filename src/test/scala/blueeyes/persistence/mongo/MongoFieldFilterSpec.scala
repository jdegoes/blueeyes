package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JPathImplicits._
import blueeyes.json._
import MongoFilterOperators._
import blueeyes.json.JsonAST._

class MongoFieldFilterSpec extends Specification{
  "creates valid json for $regex operator" in{
    import MongoFilterImplicits._
    MongoFieldFilter("foo", $regex, JObject(List(JField("$regex", JString("bar")), JField("$options", JString("i"))))).filter mustEqual (JObject(JField("foo", JObject(List(JField("$regex", JString("bar")), JField("$options", JString("i"))))) :: Nil))
  }
  "creates valid json for $eq operator" in{
    import MongoFilterImplicits._
    MongoFieldFilter("foo", $eq, "bar").filter mustEqual (JObject(JField("foo", JString("bar")) :: Nil))
  }
  "creates valid json for $near operator" in{
    import MongoFilterImplicits._
    MongoFieldFilter("foo", $near, JObject(List(JField("$near", JArray(List(JInt(1), JInt(2))))))).filter mustEqual (JObject(JField("foo", JObject(List(JField("$near", JArray(List(JInt(1), JInt(2))))))) :: Nil))
  }
  "creates valid json for $near operator with $maxDistance" in{
    import MongoFilterImplicits._
    MongoFieldFilter("foo", $near, JObject(List(JField("$near", JArray(List(JInt(1), JInt(2)))), JField("$maxDistance", JInt(10))))).filter mustEqual (JObject(JField("foo", JObject(List(JField("$near", JArray(List(JInt(1), JInt(2)))), JField("$maxDistance", JInt(10))))) :: Nil))
  }
  "creates valid json for $eq operator for empty path" in{
    import MongoFilterImplicits._
    MongoFieldFilter("", $eq, "bar").filter mustEqual (JString("bar"))
  }
  "creates valid json for $eq operator and complex path operator" in{
    import MongoFilterImplicits._
    MongoFieldFilter("author.name", $eq, "joe").filter mustEqual (JObject(JField("author.name", JString("joe")) :: Nil))
  }
  "creates valid json for another operator then $eq" in{
    import MongoFilterImplicits._
    MongoFieldFilter("foo", $ne, "bar").filter mustEqual (JObject(JField("foo", JObject(JField("$ne", JString("bar")) :: Nil)) :: Nil))
  }
  "creates valid json for another operator then $eq and complex path operator" in{
    import MongoFilterImplicits._
    MongoFieldFilter("author.name", $ne, "joe").filter mustEqual (JObject(JField("author.name", JObject(JField("$ne", JString("joe")) :: Nil)) :: Nil))
  }

  "unary_! use negative operator" in{
    import MongoFilterImplicits._
    MongoFieldFilter("foo", $eq, "bar").unary_! mustEqual(MongoFieldFilter("foo", $ne, "bar"))
  }
}