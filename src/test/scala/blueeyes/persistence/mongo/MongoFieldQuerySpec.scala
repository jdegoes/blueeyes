package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JPathImplicits._
import blueeyes.json._
import MongoQueryOperators._
import blueeyes.json.JsonAST.{JObject, JString, JField}

class MongoFieldQuerySpec extends Specification{
  "creates valid json for $eq operator" in{
    import MongoQueryImplicits._
    MongoFieldQuery("foo", $eq, "bar").query mustEqual (JObject(JField("foo", JString("bar")) :: Nil))
  }
  "creates valid json for $eq operator and complex path operator" in{
    import MongoQueryImplicits._
    MongoFieldQuery("author.name", $eq, "joe").query mustEqual (JObject(JField("author.name", JString("joe")) :: Nil))
  }
  "creates valid json for another operator then $eq" in{
    import MongoQueryImplicits._
    MongoFieldQuery("foo", $ne, "bar").query mustEqual (JObject(JField("foo", JObject(JField("$ne", JString("bar")) :: Nil)) :: Nil))
  }
  "creates valid json for another operator then $eq and complex path operator" in{
    import MongoQueryImplicits._
    MongoFieldQuery("author.name", $ne, "joe").query mustEqual (JObject(JField("author.name", JObject(JField("$ne", JString("joe")) :: Nil)) :: Nil))
  }
}