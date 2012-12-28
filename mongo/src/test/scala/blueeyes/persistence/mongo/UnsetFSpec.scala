package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import UpdateFieldFunctions._
import dsl._

class UnsetFSpec extends Specification{
  "fuse does not change update" in {
    UnsetF("n").fuseWith(SetF("n", 2)) mustEqual(Some(UnsetF("n")))
  }
}
