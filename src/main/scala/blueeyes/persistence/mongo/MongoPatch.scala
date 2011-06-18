package blueeyes.persistence.mongo

import scala.collection.immutable.ListSet
import blueeyes.json.JPath

private[mongo] object Changes {
  trait Change {
    /**
     * The list of changes that make up this change.
     */
    def flatten: ListSet[Change1]

    /**
     * Coalesces this change with that change, to produce a new change that
     * achieves the effect of both changes applied sequentially.
     */
    def *>(that: Change) = Changes.compose(flatten, that.flatten)

    /**
     * Coalesces this change with the list of changes, to produce a new
     * change that achieves the effect of both changes applied sequentially.
     */
    def *>(list: ListSet[Change]) = Changes.compose(flatten, Changelist(list).flatten)
  }

  trait Change1 extends Change {
    /**
     * The path that will be changed.
     */
    def path: JPath

    def flatten = ListSet.empty + this

    final def commuteWith(older: Change1): Option[Tuple2[Change1, Change1]] = if (older.path != this.path) Some((older, this)) else commuteWithImpl(older)

    final def fuseWith(change: Change1): Option[Change1] = if (change.path != this.path) None else fuseWithImpl(change)

    protected def commuteWithImpl(older: Change1): Option[Tuple2[Change1, Change1]] = None

    protected def fuseWithImpl(older: Change1): Option[Change1]
  }

  sealed case class Changelist[T <: Change](list: ListSet[T]) extends Change {
    lazy val flatten = list.flatMap(_.flatten)
  }

  object Changelist {
    implicit def patchToChangelist[T <: Change1](patch: T): Changelist[T] = Changelist[T](ListSet.empty + patch)

    implicit def setToChangelist[T <: Change1](list: ListSet[T]): Changelist[T] = Changelist[T](list)
  }

  def compose(older: ListSet[Change1], newer: ListSet[Change1]): ListSet[Change1] = 
    if (newer.isEmpty) older 
    else compose(_compose(newer.last, older), newer.take(newer.size - 1))

  private def _compose(c: Change1, cs: ListSet[Change1]): ListSet[Change1] = {
    if (cs.isEmpty) ListSet(c)
    else c.fuseWith(cs.head) match {
      case None => c.commuteWith(cs.head) match {
        case None => ListSet(c) ++ cs
        case Some(t) => ListSet(t._1) ++ _compose(t._2, cs.tail)
      }

      case Some(f) => ListSet(f) ++ cs.tail
    }
  }
}
