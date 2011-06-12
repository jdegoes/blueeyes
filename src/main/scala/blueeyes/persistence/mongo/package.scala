package blueeyes.persistence

import scala.collection.immutable.ListSet
import scalaz.Monoid

package object mongo extends blueeyes.persistence.mongo.MongoImplicits {
  type MockMongo = mock.MockMongo

  implicit val MongoUpdateMonoid = new Monoid[MongoUpdate] {
    val zero = MongoUpdateNothing

    def append(u1: MongoUpdate, u2: => MongoUpdate): MongoUpdate = {
      import Changes._
      import Changelist._
      import MongoUpdateObject._

      implicit def mongoUpdate(changes: ListSet[Change1]) = MongoUpdateFields(changes.map(_.asInstanceOf[MongoUpdateField]))
      def updateFields(update: MongoUpdateObject)         = MongoUpdateFields(decompose(update.value))

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
      case (x: MongoAndFilter, y: MongoAndFilter)    => MongoAndFilter(y.queries ++ x.queries.toList.reverse)
      case (_, y: MongoAndFilter) => MongoAndFilter(y.queries + f1)
      case (x: MongoAndFilter, _) => MongoAndFilter(ListSet.empty + f2 ++ x.queries.toList.reverse)
      case (MongoFilterAll, _)   => MongoAndFilter(ListSet.empty + f2)
      case (_, MongoFilterAll)   => MongoAndFilter(ListSet.empty + f1)
      case _  => MongoAndFilter(ListSet.empty + (f2, f1))
    }
  }

  implicit val MongoFilterOrMonoid = new Monoid[MongoFilter]{
    val zero = MongoFilterAll

    def append(f1: MongoFilter, f2: => MongoFilter): MongoFilter = (f1, f2) match{
      case (x: MongoOrFilter, y: MongoOrFilter)    => MongoOrFilter(y.queries ++ x.queries.toList.reverse)
      case (_, y: MongoOrFilter) => MongoOrFilter(y.queries + f1)
      case (x: MongoOrFilter, _) => MongoOrFilter(ListSet.empty + f2 ++ x.queries.toList.reverse)
      case (MongoFilterAll, _)   => f2
      case (_, MongoFilterAll)   => f1
      case _  => MongoOrFilter(ListSet.empty + (f2, f1))
    }
  }
}
