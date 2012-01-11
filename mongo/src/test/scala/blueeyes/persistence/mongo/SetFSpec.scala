package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import UpdateFieldFunctions._

class SetFSpec extends Specification{
  "fuse does not change update" in {
    SetF("n", 1).fuseWith(SetF("n", 2)) mustEqual(Some(SetF("n", 1)))
  }
}