package blueeyes.persistence.mongo

import blueeyes.json.JPath

private[mongo] object Changes {
  trait Change {
    /**
     * The list of changes that make up this change.
     */
    def flatten: List[Change1]

    /**
     * Coalesces this change with that change, to produce a new change that
     * achieves the effect of both changes applied sequentially.
     */
    def *>(that: Change) = Changes.compose(flatten, that.flatten)

    /**
     * Coalesces this change with the list of changes, to produce a new
     * change that achieves the effect of both changes applied sequentially.
     */
    def *>(list: List[Change]) = Changes.compose(flatten, Changelist(list).flatten)
  }

    trait Change1 extends Change {
    /**
     * The path that will be changed.
     */
    def path: JPath

    def flatten = List(this)

    final def commuteWith(older: Change1): Option[Tuple2[Change1, Change1]] = if (older.path != this.path) Some((older, this)) else commuteWithImpl(older)

    final def fuseWith(change: Change1): Option[Change1] = if (change.path != this.path) None else fuseWithImpl(change)

    protected def commuteWithImpl(older: Change1): Option[Tuple2[Change1, Change1]] = None

    protected def fuseWithImpl(older: Change1): Option[Change1] = None
  }

  sealed case class Changelist(list: List[Change]) extends Change {
    lazy val flatten = list.flatMap(_.flatten)
  }

  object Changelist {
    implicit def patchToChangelist(patch: Change1): Changelist = Changelist(List(patch))

    implicit def listToChangelist(list: List[Change1]): Changelist = Changelist(list)
  }

  def compose(older: List[Change1], newer: List[Change1]): List[Change1] = if (newer.isEmpty) older else compose(compose(newer.last, older), newer.take(newer.length - 1))

  private def compose(c: Change1, cs: List[Change1]): List[Change1] = if (cs.isEmpty) c :: Nil else c.fuseWith(cs.head) match {
    case None => c.commuteWith(cs.head) match {
      case None => c :: cs
      case Some(t) => t._1 :: compose(t._2, cs.tail)
    }
    case Some(f) => f :: cs.tail
  }
}