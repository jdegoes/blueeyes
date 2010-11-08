package blueeyes.core.service

/**
 * The status of a service, which can be starting, started, stopping, stopped,
 * and errored.
 */
sealed trait RunningStatus {
  def isStarting: Boolean = (this == RunningStatus.Starting)
  def isStarted:  Boolean = (this == RunningStatus.Started)
  
  def isStopping: Boolean = (this == RunningStatus.Stopping)
  def isStopped:  Boolean = (this == RunningStatus.Stopped)
  
  def isErrored: Boolean = (this == RunningStatus.Errored)
}
object RunningStatus {
  case object Starting extends RunningStatus
  case object Started extends RunningStatus
  
  case object Stopping extends RunningStatus
  case object Stopped extends RunningStatus
  
  case object Errored extends RunningStatus
}