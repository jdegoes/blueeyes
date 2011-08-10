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

    final def fuseWith(change: Change1): Option[Change1] = if (change.path != this.path) None else fuseWithImpl(change)

    protected def fuseWithImpl(older: Change1): Option[Change1]
  }

  sealed case class Changelist[T <: Change](list: ListSet[T]) extends Change {
    lazy val flatten = list.flatMap(_.flatten)
  }

  object Changelist {
    implicit def patchToChangelist[T <: Change1](patch: T): Changelist[T] = Changelist[T](ListSet.empty + patch)

    implicit def setToChangelist[T <: Change1](list: ListSet[T]): Changelist[T] = Changelist[T](list)
  }

  def compose(older: ListSet[Change1], newer: ListSet[Change1]): ListSet[Change1] = {
    val sortedOlder = older.foldLeft(Map[JPath, List[Change1]]()){(result, change) =>
      val changes = result.get(change.path).getOrElse(List[Change1]())
      result + Tuple2(change.path, changes ::: List(change))
    }

    val newChanges = newer.foldLeft(sortedOlder){(result, change) =>
      val changes = result.get(change.path).getOrElse(List[Change1]())
      result + Tuple2(change.path, _compose(change, changes) )
    }
    ListSet.empty ++ newChanges.values.flatten
  }

  private def _compose(c: Change1, cs: List[Change1]): List[Change1] = {
    if (cs.isEmpty) List(c)
    else c.fuseWith(cs.head) match {
      case Some(t) => cs.tail ::: List(t)
      case None    => _compose(c, cs.tail) ::: List(cs.head)
    }
  }
}
