package blueeyes.core.service.test

import org.specs2.mutable.Specification


class MockConfigurationSpec extends Specification {

  import MockConfiguration._

  "MockConfiguration" should {
    "return the value the global switch is set to" in {
      setGlobalSwitch(Some("foo"))
      globalSwitch mustEqual Some("foo")

      setGlobalSwitch(None)
      globalSwitch mustEqual None
    }

    "turn on global switch when requested to and property has no value" in {
      setGlobalSwitch(None)
      turnOnGlobalSwitch
      globalSwitch mustEqual Some("true")
    }

    "not change global switch if it already has a value" in {
      setGlobalSwitch(Some("false"))
      turnOnGlobalSwitch
      globalSwitch mustEqual Some("false")
    }

    "use value of global switch if key to isMocked has no value" in {
      val testKey = "blueeyes.testing.mock"
      sys.props.remove(testKey)

      setGlobalSwitch(Some("true"))
      isMocked(testKey) mustEqual true

      setGlobalSwitch(Some("false"))
      isMocked(testKey) mustEqual false

      setGlobalSwitch(None)
      isMocked(testKey) mustEqual false
    }

    "use value of key if it has one" in {
      val testKey = "blueeyes.testing.mock"
      setGlobalSwitch(None)

      sys.props.put(testKey, "true")
      isMocked(testKey) mustEqual true

      sys.props.put(testKey, "false")
      isMocked(testKey) mustEqual false

      sys.props.remove(testKey)
      isMocked(testKey) mustEqual false
    }
  }
}
