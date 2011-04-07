package blueeyes.persistence.mongo

import org.specs.Specification
import MongoUpdateOperators._
import MongoFilterOperators._
import blueeyes.json.JsonAST._

class MongoUpdateNothingSpec  extends Specification{
  "build valid json with MongoUpdateField" in {
    MongoUpdateNothing & ("x" inc (1)) mustEqual ("x" inc (1))
  }

}