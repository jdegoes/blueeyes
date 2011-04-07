package blueeyes.persistence.mongo

import org.specs.Specification
import UpdateFieldFunctions._

class UnsetFSpec extends Specification{
  "fuse does not change update" in {
    UnsetF("n").fuseWith(SetF("n", 2)) mustEqual(Some(UnsetF("n")))
  }
}