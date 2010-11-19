package blueeyes.persistence.mongo

import org.spex.Specification
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import MongoUpdateOperators._
import MongoFilterOperators._
import blueeyes.json.JsonAST._

class MongoUpdateFieldsValuesSpec extends Specification{
  "build valid json with several MongoUpdateFieldValue" in{
    import MongoFilterImplicits._
    (("x" inc (1)) & ("y" set (1))).toJValue mustEqual (JObject(JField("$inc", JObject(JField("x", JInt(1)) :: Nil)) :: JField("$set", JObject(JField("y", JInt(1)) :: Nil)) :: Nil))
  }

  "build valid json with 3 MongoUpdateFieldValues" in {
    import MongoFilterImplicits._
    (("x" inc (1)) & ("y" set (1)) & ("z" inc (1))).toJValue mustEqual (JObject(JField("$inc", JObject(JField("x", JInt(1)) :: JField("z", JInt(1)):: Nil)) :: JField("$set", JObject(JField("y", JInt(1)) :: Nil)) :: Nil))
  }
}
