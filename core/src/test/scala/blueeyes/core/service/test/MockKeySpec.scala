package blueeyes.core.service.test

import org.specs2.mutable.Specification

class MockKeySpec extends Specification {

  "GlobalKey" should {
    "return the value to which it is set, if not set" in {
      val globalMock = sys.props.get("blueeyes.mock").getOrElse("false").toBoolean

      if(globalMock) {
        // check we can't change the value
        val expected = GlobalKey.box.value
        GlobalKey.value = Some(!(GlobalKey.value))
        GlobalKey.box.value mustEqual expected
      } else {
        val old = GlobalKey.box.value
        val expected = Some(!(GlobalKey.value))
        GlobalKey.value = expected
        GlobalKey.value mustEqual expected
        GlobalKey.value = old
        // Get Specs2 to shut-up
        1 mustEqual 1
      }
    }
  }

  "MockKey" should {
    "inherit value from GlobalKey is no value set" in {
      val old = GlobalKey.box.value
      val expected = old.map(!_)
      GlobalKey.value = expected
      val key = MockKey("a.test", old.getOrElse(false))
      key.value mustEqual GlobalKey.value
      GlobalKey.value = old
      // Why is Specs2 so over-engineered?
      1 mustEqual 1
    }
  }

}
