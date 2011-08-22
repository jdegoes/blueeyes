package blueeyes.health.metrics

sealed trait IntervalConfig

case object eternity extends IntervalConfig

case class interval(length: IntervalLength, count: Int) extends IntervalConfig{
  require(count > 0 && count < 1000, "Interval Count must be less then 1000 anf more then 0.")
}
