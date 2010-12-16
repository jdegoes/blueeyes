package blueeyes.persistence.mongo

import org.spex.Specification
import UpdateFieldFunctions._

class PopLastFSpec extends Specification{
  "fuse applies pop to set update" in {
    import MongoImplicits._

    PopLastF("n").fuseWith(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo"), MongoPrimitiveString("bar")))) mustEqual(Some(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo")))))
  }
  "fuse with popLast leaves popLast" in {
    import MongoImplicits._

    PopLastF("n").fuseWith(PopLastF("n")) mustEqual(Some(PopLastF("n")))
  }
  "fuse with popFirst leaves popLast" in {
    import MongoImplicits._

    PopLastF("n").fuseWith(PopFirstF("n")) mustEqual(Some(PopLastF("n")))
  }
}