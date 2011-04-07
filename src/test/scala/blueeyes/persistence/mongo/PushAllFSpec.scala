package blueeyes.persistence.mongo

import org.specs.Specification
import UpdateFieldFunctions._

class PushAllFSpec extends Specification{
  "fuse applies pushAll to set update" in {
    PushAllF("n", List(MongoPrimitiveString("bar"))).fuseWith(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo") :: Nil))) mustEqual(Some(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo") :: MongoPrimitiveString("bar") :: Nil))))
  }
  "fuse with pushAll composes pushAll" in {
    PushAllF("n", List(MongoPrimitiveString("bar"))).fuseWith(PushAllF("n", List(MongoPrimitiveString("foo")))) mustEqual(Some(PushAllF("n", List(MongoPrimitiveString("bar"), MongoPrimitiveString("foo")))))
  }
  "fuse with push composes pushAll" in {
    PushAllF("n", List(MongoPrimitiveString("bar"))).fuseWith(PushF("n", "foo")) mustEqual(Some(PushAllF("n", List(MongoPrimitiveString("bar"), MongoPrimitiveString("foo")))))
  }
}