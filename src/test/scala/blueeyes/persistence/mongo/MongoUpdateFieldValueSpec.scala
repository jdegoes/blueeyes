package blueeyes.persistence.mongo

import org.spex.Specification
import MongoUpdateOperators._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonParser
import UpdateFieldFunctions._

class MongoUpdateFieldValueSpec  extends Specification{
  "build valid json" in {
    IncF("n", 1).toJValue mustEqual  (JObject(JField("$inc", JObject(JField("n", JInt(1)) :: Nil)) :: Nil))
  }
  "build valid json for set MongoQuery" in {
    PullF("foo", "bar" === 1).toJValue mustEqual  (JObject(JField("$pull", JObject(JField("foo", JObject(JField("bar", JInt(1)) :: Nil)) :: Nil)) :: Nil))
  }
  "build valid json for pull and for elemMatch" in {
    PullF("foo", MongoAndFilter(("bar" === 1) :: Nil).elemMatch("")).toJValue mustEqual  (JsonParser.parse(""" {"$pull": {"foo": {"$elemMatch" : {"bar": 1} }}} """))
  }
}
