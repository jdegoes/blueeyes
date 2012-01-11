package blueeyes.persistence.mongo.mock

import blueeyes.persistence.mongo.MongoImplicits._
import org.specs2.mutable.Specification

class MockMongoSpec extends Specification{
  "create database" in{
    val mongo = new MockMongo()

    mongo.database("foo") must not beNull
  }

  "return the same database for the same name" in{
    val mongo = new MockMongo()

    mongo.database("foo") must be (mongo.database("foo"))
  }
}
