package blueeyes.persistence.mongo

import org.spex.Specification
import UpdateFieldFunctions._

class IncFSpec extends Specification{
  "fuse increases set update" in {
    import MongoImplicits._

    IncF("n", 2).fuseWith(SetF("n", 3)) mustEqual(Some(SetF("n", 5)))
  }
  "fuse increases set inc update" in {
    import MongoImplicits._

    IncF("n", 2).fuseWith(IncF("n", 4)) mustEqual(Some(IncF("n", 6)))
  }
}