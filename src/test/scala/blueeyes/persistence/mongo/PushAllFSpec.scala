package blueeyes.persistence.mongo

import org.spex.Specification
import UpdateFieldFunctions._

class PushAllFSpec extends Specification{
  "fuse applies pushAll to set update" in {
    import MongoImplicits._

    PushAllF("n", List(MongoPrimitiveString("bar"))).fuseWith(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo")))) mustEqual(Some(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo"), MongoPrimitiveString("bar")))))
  }
  "fuse with pushAll leaves pushAll" in {
    import MongoImplicits._

    PushAllF("n", List(MongoPrimitiveString("bar"))).fuseWith(PushAllF("n", List(MongoPrimitiveString("foo")))) mustEqual(Some(PushAllF("n", List(MongoPrimitiveString("bar")))))
  }
}