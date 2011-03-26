package blueeyes.persistence.mongo

/**
 * Simple abstraction for representing a bunch of patches to a mongo collection.
 */
case class MongoPatches(patches: Map[MongoFilter, MongoUpdate]) {
  def append(that: MongoPatches): MongoPatches = this :+ that

  /** Combines the two patches into a single patches object.
   */
  def :+ (that: MongoPatches): MongoPatches = {
    val allFilters = this.patches.keys ++ that.patches.keys

    val empty = Map.empty[MongoFilter, MongoUpdate]

    MongoPatches(allFilters.foldLeft(empty) { (patches, filter) =>
      val update1 = this.patches.get(filter).getOrElse(MongoUpdateNothing)
      val update2 = that.patches.get(filter).getOrElse(MongoUpdateNothing)

      val totalUpdate = update1 & update2

      patches + (filter -> totalUpdate)
    })
  }
}