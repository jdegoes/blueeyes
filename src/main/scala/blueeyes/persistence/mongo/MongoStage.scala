package blueeyes.persistence.mongo

import blueeyes.concurrent.FutureDeliveryStrategy
import blueeyes.json.JsonAST._
import blueeyes.persistence.cache.{ExpirationPolicy, Stage}

case class MongoStageSettings(expirationPolicy: ExpirationPolicy, maximumCapacity: Int)

/** A stage for updates to Mongo.
 */
class MongoStage (val database: MongoDatabase, val collection: MongoCollection, mongoStageSettings: MongoStageSettings)(implicit futureDeliveryStrategy: FutureDeliveryStrategy) 
extends Stage[MongoFilter, MongoUpdate] {

  def flush(filter: MongoFilter, update: MongoUpdate) = {
    database[JNothing.type] {
      upsert(collection).set(update).where(filter)
    }
  }

  def expirationPolicy = mongoStageSettings.expirationPolicy

  def maximumCapacity = mongoStageSettings.maximumCapacity
}

object MongoStage {
  def apply(database: MongoDatabase, collection: MongoCollection, mongoStageSettings: MongoStageSettings)(implicit futureDeliveryStrategy: FutureDeliveryStrategy) = {
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
