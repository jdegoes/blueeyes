package blueeyes.health.metrics.histogram

import blueeyes.health.metrics.Timer

trait ValueStrategy[V] {
  def plus(value: V, stat: Long): V

  def zero: V

  def count(value: V): Long
}


object ValueStrategy{
  implicit val DoubleValueStrategy = new DoubleValueStrategy

  implicit val LongValueStrategy   = new LongValueStrategy

  implicit val TimerValueStrategy  = new TimerValueStrategy
}

class DoubleValueStrategy extends ValueStrategy[Double]{
  def plus(value: Double, stat: Long) = value + stat

  def zero = 0.0

  def count(value: Double) = value.toLong
}

class LongValueStrategy extends ValueStrategy[Long]{
  def plus(value: Long, stat: Long) = value + stat

  def zero = 0l

  def count(value: Long) = value
}

class TimerValueStrategy  extends ValueStrategy[Timer]{
  def plus(value: Timer, ns: Long) = value.+=(ns)

  def zero = new Timer()

  def count(value: Timer) = value.count
}