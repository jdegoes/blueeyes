package blueeyes.persistence.mongo

import org.spex.Specification
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import MongoUpdateOperators._
import MongoFilterOperators._
import blueeyes.json.JsonAST._

class MongoUpdateNothingSpec  extends Specification{
  "build valid json with MongoUpdateFieldValue" in {
    import MongoFilterImplicits._
    MongoUpdateNothing & ("x" inc (1)) mustEqual ("x" inc (1))
  }

}