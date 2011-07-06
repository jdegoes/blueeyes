package blueeyes
package persistence.mongo

import blueeyes.concurrent.Future
import blueeyes.json.JsonAST._
import scalaz._
import Scalaz._

/**
 * Simple abstraction for representing a collections of MongoDB patches.
 */
case class MongoPatches(patches: Map[MongoFilter, MongoUpdate]) {
  def append(that: MongoPatches): MongoPatches = this ++ that

  /** Combines the two collections of patches into a single collection.
   */
  def ++ (that: MongoPatches): MongoPatches = {
    MongoPatches(this.patches <+> that.patches)
  }

  /** Adds a single patch to this collection of patches.
   */
  def + (patch: (MongoFilter, MongoUpdate)): MongoPatches = copy(
    patches = this.patches <+> Map(patch)
  )

  /** Commits all patches to the database and returns a future that completes
   * if and only if all of the patches succeed.
    */
  def commit(database: Database, collection: MongoCollection): Future[Unit] = {
    val futures = patches.toList.map { 
      case (filter, update) =>
        database[JNothing.type] {
          upsert(collection).set(update).where(filter)
        }.toUnit
    }

    Future(futures: _*).toUnit
  }
}

object MongoPatches {
  val empty: MongoPatches = MongoPatches(Map.empty[MongoFilter, MongoUpdate])

  def apply(patch: (MongoFilter, MongoUpdate)): MongoPatches = MongoPatches(Map(patch))

  def apply(iter: Iterable[(MongoFilter, MongoUpdate)]): MongoPatches = iter.foldLeft(empty) { _ + _ }

  def apply(varargs: (MongoFilter, MongoUpdate)*): MongoPatches = apply(varargs: Iterable[(MongoFilter, MongoUpdate)])
}
