package blueeyes.persistence.mongo

import org.spex.Specification
import UpdateFieldFunctions._

class PullFSpec extends Specification{
  "fuse applies pull to set update" in {
    import MongoImplicits._

    PullF("n", "" === "foo").fuseWith(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("bar"), MongoPrimitiveString("foo")))) mustEqual(Some(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("bar")))))
  }
  "fuse with pull leaves pull" in {
    import MongoImplicits._

    PullF("n", "" === "foo").fuseWith(PullF("n", "" === "bar")) mustEqual(Some(PullF("n", "" === "foo")))
  }
}