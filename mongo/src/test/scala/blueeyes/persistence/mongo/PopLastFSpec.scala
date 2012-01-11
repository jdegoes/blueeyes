package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import UpdateFieldFunctions._

class PopLastFSpec extends Specification{
  "fuse applies pop to set update" in {
    PopLastF("n").fuseWith(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo") :: MongoPrimitiveString("bar") :: Nil))) mustEqual(Some(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo") :: Nil))))
  }
  "fuse with popLast leaves popLast" in {
    PopLastF("n").fuseWith(PopLastF("n")) mustEqual(Some(PopLastF("n")))
  }
  "fuse with popFirst leaves popLast" in {
    PopLastF("n").fuseWith(PopFirstF("n")) mustEqual(Some(PopLastF("n")))
  }
}