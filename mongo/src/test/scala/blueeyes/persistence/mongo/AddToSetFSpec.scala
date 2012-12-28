package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json.JPath
import UpdateFieldFunctions._
import dsl._

class AddToSetFSpec extends Specification{
  "fuse applies addtoSet to set update" in {
    "n".addToSet(MongoPrimitiveString("bar")).fuseWith(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo") :: Nil))) mustEqual(Some(SetF("n", MongoPrimitiveArray(MongoPrimitiveString("foo") :: MongoPrimitiveString("bar") :: Nil))))
  }
  "fuse with AddToSetF(String) creates AddToSetF(String) with all elements" in {
    "n".addToSet(MongoPrimitiveString("bar")).fuseWith(JPath("n").addToSet(MongoPrimitiveString("foo"))) must beSome((JPath("n").addToSet(MongoPrimitiveString("bar"), MongoPrimitiveString("foo"))))
  }
  "fuse with AddToSetF(String) with the same strings creates AddToSetF(String) with one elements" in {
    "n".addToSet(MongoPrimitiveString("bar")).fuseWith(JPath("n").addToSet(MongoPrimitiveString("bar"))) must beSome((JPath("n").addToSet(MongoPrimitiveString("bar"))))
  }
  "fuse with AddToSetF(Array) creates AddToSetF(String) with all elements" in {
    "n".addToSet(MongoPrimitiveString("bar"), MongoPrimitiveString("foo")).fuseWith(JPath("n").addToSet(MongoPrimitiveString("baz"))) must beSome((JPath("n").addToSet(MongoPrimitiveString("bar"), MongoPrimitiveString("foo"), MongoPrimitiveString("baz"))))
  }
  "fuse with AddToSetF(String) creates AddToSetF(Array) with all elements" in {
    "n".addToSet(MongoPrimitiveString("baz")).fuseWith(JPath("n").addToSet(MongoPrimitiveString("bar"), MongoPrimitiveString("foo"))) must beSome((JPath("n").addToSet(MongoPrimitiveString("baz"), MongoPrimitiveString("bar"), MongoPrimitiveString("foo"))))
  }
  "fuse with AddToSetF(Array) creates AddToSetF(Array) with all elements" in {
    "n".addToSet(MongoPrimitiveString("bar"), MongoPrimitiveString("foo")).fuseWith(JPath("n").addToSet(MongoPrimitiveString("baz"), MongoPrimitiveString("bar baz"))) must beSome((JPath("n").addToSet(MongoPrimitiveString("bar"), MongoPrimitiveString("foo"), MongoPrimitiveString("baz"), MongoPrimitiveString("bar baz"))))
  }
}
