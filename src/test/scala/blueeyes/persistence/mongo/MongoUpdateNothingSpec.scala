package blueeyes.persistence.mongo

import org.spex.Specification
import MongoUpdateOperators._
import MongoFilterOperators._
import blueeyes.json.JsonAST._

class MongoUpdateNothingSpec  extends Specification{
  "build valid json with MongoUpdateField" in {
    import MongoImplicits._
    MongoUpdateNothing & ("x" inc (1)) mustEqual ("x" inc (1))
  }

}