package blueeyes.persistence.mongo

import org.specs.Specification
import UpdateFieldFunctions._
import com.mongodb.MongoException

class PullAllFSpec extends Specification{
  "fuse applies pullAll to set update" in {
    PullAllF("n", List(MongoPrimitiveString("bar"))).fuseWith(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("bar") :: MongoPrimitiveString("foo") :: Nil))) mustEqual(Some(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo") :: Nil))))
  }
  "fuse with pullAll composes pullAll" in {
    PullAllF("n", List(MongoPrimitiveString("bar"))).fuseWith(PullAllF("n", List(MongoPrimitiveString("foo")))) mustEqual(Some(PullAllF("n", List(MongoPrimitiveString("bar"), MongoPrimitiveString("foo")))))
  }
  "fuse with pull (with '' === value filter) composes pullAll" in {
    PullAllF("n", List(MongoPrimitiveString("bar"))).fuseWith(PullF("n", "" === "foo")) mustEqual(Some(PullAllF("n", List(MongoPrimitiveString("bar"), MongoPrimitiveString("foo")))))
  }
  "fuse with pull (with another then '' === value) filter fails" in {
    PullAllF("n", List(MongoPrimitiveString("bar"))).fuseWith(PullF("n", "bar" === "foo")) must throwA[RuntimeException]
  }
}