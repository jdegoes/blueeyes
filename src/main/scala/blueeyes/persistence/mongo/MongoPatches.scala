package blueeyes.persistence.mongo

import blueeyes.concurrent.{Future, FutureDeliveryStrategySequential}
import blueeyes.json.JsonAST._

/**
 * Simple abstraction for representing a collections of MongoDB patches.
 */
case class MongoPatches(patches: Map[MongoFilter, MongoUpdate]) extends FutureDeliveryStrategySequential {
  def append(that: MongoPatches): MongoPatches = this ++ that

  /** Combines the two collections of patches into a single collection.
   */
  def ++ (that: MongoPatches): MongoPatches = {
    // TODO: Start with larger map!
    MongoPatches(that.patches.foldLeft(this.patches) { (allPatches, tuple) =>
      val (filter, update2) = tuple

      val update = allPatches.get(filter).map(_ & update2).getOrElse(update2)

      allPatches + (filter -> update)
    })
  }

  /** Adds a single patch to this collection of patches.
   */
  def + (patch: (MongoFilter, MongoUpdate)): MongoPatches = copy(
    patches = patches + (patch._1 -> patches.get(patch._1).map(_ & patch._2).getOrElse(patch._2))
  )

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
    patches + patch
  }

  def apply(varargs: (MongoFilter, MongoUpdate)*): MongoPatches = apply(varargs: Iterable[(MongoFilter, MongoUpdate)])
}