package blueeyes.persistence.mongo

import org.specs.Specification
import UpdateFieldFunctions._

class PushFSpec extends Specification{
  "fuse applies push to set update" in {
    PushF("n", "bar").fuseWith(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo") :: Nil))) mustEqual(Some(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo") :: MongoPrimitiveString("bar") :: Nil))))
  }
  "fuse with push composes push" in {
    PushF("n", "bar").fuseWith(PushF("n", "foo")) mustEqual(Some(PushAllF("n", List(MongoPrimitiveString("bar"), MongoPrimitiveString("foo")))))
  }
  "fuse withAll push composes push" in {
    PushF("n", "bar").fuseWith(PushAllF("n", List(MongoPrimitiveString("foo")))) mustEqual(Some(PushAllF("n", List(MongoPrimitiveString("bar"), MongoPrimitiveString("foo")))))
  }
}