package blueeyes.persistence

import scalaz.Monoid

package object mongo extends blueeyes.persistence.mongo.MongoImplicits {
  type MongoValueRepr = AnyRef {type Values; def values: Values}
  type MockMongo = mock.MockMongo

  implicit val MongoUpdateMonoid = new Monoid[MongoUpdate] {
    val zero = MongoUpdateNothing

    def append(u1: MongoUpdate, u2: => MongoUpdate): MongoUpdate = {
      import Changes._
      import Changelist._
      import MongoUpdateObject._

      implicit def mongoUpdate(changes: Seq[Change1]) = MongoUpdateFields(changes.map(_.asInstanceOf[MongoUpdateField]))
      def updateFields(update: MongoUpdateObject)     = MongoUpdateFields(decompose(update.value))

      (u1, u2) match {
        case (MongoUpdateNothing, _)  => u2
        case (_, MongoUpdateNothing)  => u1

        case (x: MongoUpdateField,   y: MongoUpdateField)  => x *> y
        case (x: MongoUpdateField,   y: MongoUpdateFields) => x *> y.list

        case (x: MongoUpdateFields, y: MongoUpdateField)   => Changelist(x.list) *> y
        case (x: MongoUpdateFields, y: MongoUpdateFields)  => Changelist(x.list) *> y.list

        case (x: MongoUpdateObject, _)   => append(updateFields(x), u2)
        case (_, y: MongoUpdateObject)   => append(u1, updateFields(y))
      }
    }
  }

  implicit val MongoFilterAndMonoid = new Monoid[MongoFilter]{
    val zero = MongoFilterAll

    def append(f1: MongoFilter, f2: => MongoFilter): MongoAndFilter = (f1, f2) match{
      case (x: MongoAndFilter, y: MongoAndFilter)    => MongoAndFilter(x.queries ++ y.queries)
      case (_, y: MongoAndFilter) => MongoAndFilter(f1 +: y.queries)
      case (x: MongoAndFilter, _) => MongoAndFilter(x.queries :+ f2)
      case (MongoFilterAll, _)   => MongoAndFilter(List(f2))
      case (_, MongoFilterAll)   => MongoAndFilter(List(f1))
      case _  => MongoAndFilter(List(f1, f2))
    }
  }

  implicit val MongoFilterOrMonoid = new Monoid[MongoFilter]{
    val zero = MongoFilterAll

    def append(f1: MongoFilter, f2: => MongoFilter): MongoFilter = (f1, f2) match{
      case (x: MongoOrFilter, y: MongoOrFilter)    => MongoOrFilter(x.queries ++ y.queries)
      case (_, y: MongoOrFilter) => MongoOrFilter(f1 +: y.queries)
      case (x: MongoOrFilter, _) => MongoOrFilter(x.queries :+ f2)
      case (MongoFilterAll, _)   => f2
      case (_, MongoFilterAll)   => f1
      case _  => MongoOrFilter(List(f1, f2))
    }
  }
}
