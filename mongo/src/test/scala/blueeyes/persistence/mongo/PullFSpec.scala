package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import UpdateFieldFunctions._

class PullFSpec extends Specification{
  "fuse applies pull to set update" in {
    PullF("n", "" === "foo").fuseWith(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("bar") :: MongoPrimitiveString("foo") :: Nil))) mustEqual(Some(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("bar") :: Nil))))
  }
  "fuse with pull (with '' === value filter) composes pull" in {
    PullF("n", "" === "foo").fuseWith(PullF("n", "" === "bar")) mustEqual(Some(PullAllF("n", List(MongoPrimitiveString("foo"), MongoPrimitiveString("bar")))))
  }
  "fuse with pull (with another then '' === value) fails" in {
    PullF("n", "" === "foo").fuseWith(PullF("n", "foo" === "bar")) must throwA[RuntimeException]
  }
  "fuse with pullAll (with '' === value filter) composes pull" in {
    PullF("n", "" === "foo").fuseWith(PullAllF("n", List(MongoPrimitiveString("bar")))) mustEqual(Some(PullAllF("n", List(MongoPrimitiveString("foo"), MongoPrimitiveString("bar")))))
  }
}