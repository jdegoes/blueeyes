package blueeyes.persistence.mongo

import scala.collection.mutable.Map

import blueeyes.json.JsonAST._

import blueeyes.persistence.cache.{ExpirationPolicy, CacheSettings, Stage}

case class MongoStageSettings(expirationPolicy: ExpirationPolicy, initialCapacity: Int, maximumCapacity: Int)

/** A stage for updates to Mongo.
 */
class MongoStage(database: MongoDatabase, collection: MongoCollection, mongoStageSettings: MongoStageSettings) extends
  Stage[MongoFilter, MongoUpdate] {
  final val coalesce = (f: MongoFilter, u1: MongoUpdate, u2: MongoUpdate) => u1 & u2

  final val settings = CacheSettings[MongoFilter, MongoUpdate](
    expirationPolicy        = mongoStageSettings.expirationPolicy,
    initialCapacity         = mongoStageSettings.initialCapacity,
    maximumWeightedCapacity = mongoStageSettings.maximumCapacity,
    evict                   = (filter: MongoFilter, update: MongoUpdate) => {
      database[JNothing.type] {
        upsert(collection).set(update).where(filter)
      }
    }
  )
}

object MongoStage {
  def apply(database: MongoDatabase, collection: MongoCollection, mongoStageSettings: MongoStageSettings) = {
    new MongoStage(database, collection, mongoStageSettings)
  }
}