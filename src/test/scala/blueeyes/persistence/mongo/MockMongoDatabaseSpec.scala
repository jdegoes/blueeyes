package blueeyes.persistence.mongo

import org.specs.Specification

class MockMongoDatabaseSpec extends Specification{
  "create collection" in{
    val mongo = new MockMongo()

    mongo.database("foo").collection("bar") must notBeNull
  }
  "return the same collection for the same name" in{
    val mongo = new MockMongo()

    mongo.database("foo").collection("bar") must be (mongo.database("foo").collection("bar"))
  }
}