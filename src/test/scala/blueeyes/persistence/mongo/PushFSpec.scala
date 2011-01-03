package blueeyes.persistence.mongo

import org.spex.Specification
import UpdateFieldFunctions._

class PushFSpec extends Specification{
  "fuse applies push to set update" in {
    import MongoImplicits._

    PushF("n", "bar").fuseWith(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo")))) mustEqual(Some(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo"), MongoPrimitiveString("bar")))))
  }
  "fuse with push composes push" in {
    import MongoImplicits._

    PushF("n", "bar").fuseWith(PushF("n", "foo")) mustEqual(Some(PushAllF("n", List(MongoPrimitiveString("bar"), MongoPrimitiveString("foo")))))
  }
  "fuse withAll push composes push" in {
    import MongoImplicits._

    PushF("n", "bar").fuseWith(PushAllF("n", List(MongoPrimitiveString("foo")))) mustEqual(Some(PushAllF("n", List(MongoPrimitiveString("bar"), MongoPrimitiveString("foo")))))
  }
}