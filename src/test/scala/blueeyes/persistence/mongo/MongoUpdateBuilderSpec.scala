package blueeyes.persistence.mongo

import org.spex.Specification
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import MongoUpdateOperators._
import blueeyes.json.JsonAST._

class MongoUpdateBuilderSpec  extends Specification{
  "builds $inc operation" in {
    import MongoFilterImplicits._
    "n" inc (1) mustEqual (MongoUpdateFieldValue("n", $inc, JInt(1)))  
  }
  "builds $set operation" in {
    import MongoFilterImplicits._
    "n" set (1) mustEqual (MongoUpdateFieldValue("n", $set, JInt(1)))  
  }
  "builds $unset operation" in {
    import MongoFilterImplicits._
    ("n" unset) mustEqual (MongoUpdateFieldValue("n", $unset, JInt(1)))  
  }
}