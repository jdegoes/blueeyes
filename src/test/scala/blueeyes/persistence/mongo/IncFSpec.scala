package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import UpdateFieldFunctions._

class IncFSpec extends Specification{
  "fuse increases set update" in {
    IncF("n", 2).fuseWith(SetF("n", 3)) mustEqual(Some(SetF("n", 5)))
  }
  "fuse increases set inc update" in {
    IncF("n", 2).fuseWith(IncF("n", 4)) mustEqual(Some(IncF("n", 6)))
  }
}