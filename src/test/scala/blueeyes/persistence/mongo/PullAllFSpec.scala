package blueeyes.persistence.mongo

import org.spex.Specification
import UpdateFieldFunctions._

class PullAllFSpec extends Specification{
  "fuse applies pullAll to set update" in {
    import MongoImplicits._

    PullAllF("n", List(MongoPrimitiveString("bar"))).fuseWith(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("bar"), MongoPrimitiveString("foo")))) mustEqual(Some(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo")))))
  }
  "fuse with pullAll leaves pullAll" in {
    import MongoImplicits._

    PullAllF("n", List(MongoPrimitiveString("bar"))).fuseWith(PullAllF("n", List(MongoPrimitiveString("foo")))) mustEqual(Some(PullAllF("n", List(MongoPrimitiveString("bar")))))
  }
}