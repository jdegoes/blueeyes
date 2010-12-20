package blueeyes.persistence.mongo

import org.spex.Specification
import UpdateFieldFunctions._

class AddToSetFSpec extends Specification{
  "fuse applies addtoSet to set update" in {
    import MongoImplicits._

    AddToSetF("n", MongoPrimitiveArray(MongoPrimitiveString("bar")), "" === "foo").fuseWith(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo")))) mustEqual(Some(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo"), MongoPrimitiveString("bar")))))
  }
  "fuse with AddToSetF leaves AddToSetF" in {
    import MongoImplicits._

    AddToSetF("n", MongoPrimitiveArray(MongoPrimitiveString("bar")), "" === "foo").fuseWith(AddToSetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo")), "" === "foo")) mustEqual(Some(AddToSetF("n", MongoPrimitiveArray(MongoPrimitiveString("bar")), "" === "foo")))
  }
}