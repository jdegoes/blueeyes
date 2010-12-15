package blueeyes.persistence.mongo

import org.spex.Specification
import UpdateFieldFunctions._

class PushFSpec extends Specification{
  "fuse applies push to set update" in {
    import MongoImplicits._

    PushF("n", "bar").fuseWith(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo")))) mustEqual(Some(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo"), MongoPrimitiveString("bar")))))
  }
  "fuse with push leaves push" in {
    import MongoImplicits._

    PushF("n", "bar").fuseWith(PushF("n", "foo")) mustEqual(Some(PushF("n", "bar")))
  }
}