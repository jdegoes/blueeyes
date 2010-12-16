package blueeyes.health.metrics

import blueeyes.json.JsonAST.JValue

trait Statistic[T, V]{  

  def +=(element: T): this.type

  def ++=(xs : scala.collection.TraversableOnce[T]): this.type = {
    xs foreach +=

    this
  }

  def count: Long

  def details: V

  def toJValue: JValue
}