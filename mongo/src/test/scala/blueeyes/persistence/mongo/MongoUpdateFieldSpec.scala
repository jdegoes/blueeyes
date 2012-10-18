package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import MongoUpdateOperators._
import blueeyes.json._
import blueeyes.json.JParser
import UpdateFieldFunctions._

class MongoUpdateFieldSpec  extends Specification{
  "build valid json" in {
    IncF("n", 1).toJValue mustEqual  (JObject(JField("$inc", JObject(JField("n", JNum(1)) :: Nil)) :: Nil))
  }
  "build valid json for set MongoQuery" in {
    PullF("foo", "bar" === 1).toJValue mustEqual  (JObject(JField("$pull", JObject(JField("foo", JObject(JField("bar", JNum(1)) :: Nil)) :: Nil)) :: Nil))
  }
  "build valid json for pull and for elemMatch" in {
    PullF("foo", MongoAndFilter(List("bar" === 1)).elemMatch("")).toJValue mustEqual  (JParser.parse(""" {"$pull": {"foo": {"$elemMatch" : {"bar": 1} }}} """))
  }
}
