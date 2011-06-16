package blueeyes.persistence.mongo

import blueeyes.json.JsonAST._
import blueeyes.persistence.cache.{ExpirationPolicy, Stage}

case class MongoStageSettings(expirationPolicy: ExpirationPolicy, maximumCapacity: Int)

/** A stage for updates to Mongo.
 */
class MongoStage (val database: MongoDatabase, val collection: MongoCollection, mongoStageSettings: MongoStageSettings)
extends Stage[MongoFilter, MongoUpdate] {
  def flush(filter: MongoFilter, update: MongoUpdate) = {
    database.unverified[JNothing.type] {
      upsert(collection).set(update).where(filter)
    }
  }

  def expirationPolicy = mongoStageSettings.expirationPolicy

  def maximumCapacity = mongoStageSettings.maximumCapacity
}

object MongoStage {
  def apply(database: MongoDatabase, collection: MongoCollection, mongoStageSettings: MongoStageSettings) = {
    new MongoStage(database, collection, mongoStageSettings)
  }
}

package functional {
  import blueeyes.persistence.cache.functional.Stage

  object MongoStage {
    def apply(baseCapacity: Int, maxCapacity: Int) = Stage.empty[MongoFilter, MongoUpdate](baseCapacity, maxCapacity)

    def apply(capacity: Int) = Stage.empty[MongoFilter, MongoUpdate](capacity)
  }
}
