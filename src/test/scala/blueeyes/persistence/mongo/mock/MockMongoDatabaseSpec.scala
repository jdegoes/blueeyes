package blueeyes.persistence.mongo.mock

import org.specs.Specification

class MockMongoDatabaseSpec extends Specification{
  private val mongo     = new MockMongo()

  "create collection" in{
    database.collection("bar") must notBeNull
  }
  "return the same collection for the same name" in{
    database.collection("bar") must be (database.collection("bar"))
  }

  private def database = mongo.database("foo").asInstanceOf[MockMongoDatabase]
}