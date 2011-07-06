package blueeyes.persistence.mongo

import blueeyes.json.JsonAST._
import blueeyes.persistence.cache.{ExpirationPolicy, Stage}

case class MongoStageSettings(expirationPolicy: ExpirationPolicy, maximumCapacity: Int)

/** A stage for updates to Mongo.
 */
class MongoStage (val database: Database, mongoStageSettings: MongoStageSettings) extends Stage[MongoFilterCollection, MongoUpdate] {
  def flush(filter: MongoFilterCollection, update: MongoUpdate) = {
    database.unverified[JNothing.type] {
      upsert(filter.collection).set(update).where(filter.filter)
    }
  }

  def expirationPolicy = mongoStageSettings.expirationPolicy

  def maximumCapacity = mongoStageSettings.maximumCapacity
}

object MongoStage {
  def apply(database: Database, mongoStageSettings: MongoStageSettings) = {
    new MongoStage(database, mongoStageSettings)
  }
}

package functional {
  import blueeyes.persistence.cache.functional.Stage

  object MongoStage {
    def apply(baseCapacity: Int, maxCapacity: Int) = Stage.empty[MongoFilter, MongoUpdate](baseCapacity, maxCapacity)

    def apply(capacity: Int) = Stage.empty[MongoFilter, MongoUpdate](capacity)
  }
}
