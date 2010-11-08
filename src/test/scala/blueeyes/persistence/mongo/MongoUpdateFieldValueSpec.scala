package blueeyes.persistence.mongo

import org.spex.Specification
import blueeyes.json.JPathImplicits._
import MongoUpdateOperators._
import blueeyes.json.JsonAST._

class MongoUpdateFieldValueSpec  extends Specification{
  "build valid json" in {
    import MongoFilterImplicits._
    MongoUpdateFieldValue($inc, "n" === 1).toJValue mustEqual  (JObject(JField("$inc", JObject(JField("n", JInt(1)) :: Nil)) :: Nil))
  }
}