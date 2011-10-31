package blueeyes.persistence.mongo

import blueeyes.health.HealthMonitor
import blueeyes.json.JsonAST._
import blueeyes.persistence.cache.{ExpirationPolicy, Stage}

case class MongoStageSettings(expirationPolicy: ExpirationPolicy, maximumCapacity: Int)

/** A stage for updates to Mongo.
 */
class MongoStage (val database: Database, mongoStageSettings: MongoStageSettings, monitor: HealthMonitor = HealthMonitor.Noop) 
extends Stage[MongoFilterCollection, MongoUpdate](monitor) {
  def flush(filter: MongoFilterCollection, update: MongoUpdate) = {
    database.unverified {
      upsert(filter.collection).set(update).where(filter.filter)
    }
  }

  def expirationPolicy = mongoStageSettings.expirationPolicy

  def maximumCapacity = mongoStageSettings.maximumCapacity
}

object MongoStage {
  def apply(database: Database, mongoStageSettings: MongoStageSettings, monitor: HealthMonitor = HealthMonitor.Noop) = {
    new MongoStage(database, mongoStageSettings, monitor)
  }
}

package functional {
  import blueeyes.persistence.cache.functional.Stage

  object MongoStage {
    def apply(baseCapacity: Int, maxCapacity: Int) = Stage.empty[MongoFilter, MongoUpdate](baseCapacity, maxCapacity)

    def apply(capacity: Int) = Stage.empty[MongoFilter, MongoUpdate](capacity)
  }
}
