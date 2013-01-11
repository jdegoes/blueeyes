package blueeyes.persistence.mongo

import blueeyes.health.HealthMonitor
import blueeyes.json._
import blueeyes.persistence.cache.{ExpirationPolicy, Stage}
import akka.util.Timeout

case class MongoStageSettings(expirationPolicy: ExpirationPolicy, maximumCapacity: Int, flushTimeout: Timeout)

/** A stage for updates to Mongo.
 */
class MongoStage (val database: Database, mongoStageSettings: MongoStageSettings, monitor: HealthMonitor = HealthMonitor.Noop) 
extends Stage[MongoFilterCollection, MongoUpdate](monitor) {
  implicit val flushTimeout = mongoStageSettings.flushTimeout

  def flush(filter: MongoFilterCollection, update: MongoUpdate): Unit = {
    database.unverified {
      dsl.upsert(filter.collection).set(update).where(filter.filter)
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

/*
package functional {
  import blueeyes.persistence.cache.functional.Stage

  object MongoStage {
    def apply(baseCapacity: Int, maxCapacity: Int) = Stage.empty[MongoFilter, MongoUpdate](baseCapacity, maxCapacity)

    def apply(capacity: Int) = Stage.empty[MongoFilter, MongoUpdate](capacity)
  }
}
*/
