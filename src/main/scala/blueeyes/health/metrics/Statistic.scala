package blueeyes.health.metrics

import blueeyes.bkka.AkkaDefaults
import blueeyes.json.JsonAST.JValue
import akka.dispatch.Future
import akka.dispatch.Promise
import akka.util.Timeout

sealed trait Statistic[-T]{
  def +=(element: T): this.type

  def ++=(xs : scala.collection.TraversableOnce[T]): this.type = {
    xs foreach +=
    this
  }
}

trait SyncStatistic[-T, V] extends Statistic[T] {
  def count: Long

  def details: V

  def toJValue: JValue
}

trait AsyncStatistic[-T, V] extends Statistic[T] with AkkaDefaults {
  def count: Future[Long]

  def details: Future[V]

  def toJValue: Future[JValue]

  def shutdown(timeout: Timeout): Future[Any]
}

abstract class WrapAsyncStatistic[-A, B](sync: SyncStatistic[A, B]) extends AsyncStatistic[A, B] {
  def +=(element: A): this.type = {
    sync += element
    this
  }

  def count: Future[Long] = Promise.successful(sync.count)
  def details: Future[B] = Promise.successful(sync.details)
  def shutdown(timeout: Timeout = Timeout.zero): Future[Any] = Promise.successful(())
}
