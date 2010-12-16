package blueeyes.persistence.mongo

import org.spex.Specification
import UpdateFieldFunctions._

class SetFSpec extends Specification{
  "fuse does not change update" in {
    import MongoImplicits._

    SetF("n", 1).fuseWith(SetF("n", 2)) mustEqual(Some(SetF("n", 1)))
  }
}