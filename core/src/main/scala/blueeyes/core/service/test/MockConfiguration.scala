package blueeyes.core.service.test

object MockConfiguration {

  /** The global switch that turns mocking on and off. Can be overridden by mockable object specific switches. See isMocked below. */
  val GlobalSwitch = "blueeyes.mock"

  def globalSwitch =
    sys.props.get(GlobalSwitch)

  /** Sets the global switch on, iff it has not already been set to a value */
  def turnOnGlobalSwitch =
    sys.props.getOrElseUpdate(GlobalSwitch, "true")

  def setGlobalSwitch(switch: Option[String]) =
    switch match {
      case Some(x) => sys.props.put(GlobalSwitch, x)
      case None => sys.props.remove(GlobalSwitch)
    }

  /** Determine if a mockable object should be mocked.
   *
   * Inspect first the given key. If it is not set, inspect the global key
   */
  def isMocked(key: String): Boolean =
    sys.props.getOrElse(key, globalSwitch.getOrElse("false")).toBoolean

}
