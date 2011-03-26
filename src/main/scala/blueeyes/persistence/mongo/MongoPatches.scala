package blueeyes.persistence.mongo

import blueeyes.concurrent.{Future, FutureDeliveryStrategySequential}
import blueeyes.json.JsonAST._

/**
 * Simple abstraction for representing a bunch of patches to a mongo collection.
 */
case class MongoPatches(patches: Map[MongoFilter, MongoUpdate]) extends FutureDeliveryStrategySequential {
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

  def +: (that: MongoPatches): MongoPatches = this.append(that)

  /** Commits all patches to the database and returns a future that completes
   * if and only if all of the patches succeed.
    */
  def commit(database: MongoDatabase, collection: MongoCollection): Future[Unit] = {
    Future((patches.toList.map { patch =>
      val (filter, update) = patch

      database[JNothing.type] {
        upsert(collection).set(update).where(filter)
      }.map(_ => ())
    }): _*).map(_ => ())
  }
}

object MongoPatches {
  val empty: MongoPatches = MongoPatches(Map.empty[MongoFilter, MongoUpdate])

  def apply(patch: (MongoFilter, MongoUpdate)): MongoPatches = MongoPatches(Map(patch))

  def apply(iter: Iterable[(MongoFilter, MongoUpdate)]): MongoPatches = iter.foldLeft(empty) { (patches, patch) =>
    patches :+ apply(patch)
  }

  def apply(varargs: (MongoFilter, MongoUpdate)*): MongoPatches = apply(varargs: Iterable[(MongoFilter, MongoUpdate)])
}