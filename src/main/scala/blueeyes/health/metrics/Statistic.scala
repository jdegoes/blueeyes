package blueeyes.health.metrics

import blueeyes.json.JsonAST.JValue
import blueeyes.concurrent.Future

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

trait AsyncStatistic[-T, V] extends Statistic[T] {
  def count: Future[Long]

  def details: Future[V]

  def toJValue: Future[JValue]

  def shutdown: akka.dispatch.Future[Any]
}
