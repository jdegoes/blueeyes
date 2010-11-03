package blueeyes.persistence.mongo

import org.specs.Specification
import MockMongoImplementation._

class MockMongoSpec extends Specification{
  "create database" in{
    val mongo = new MockMongo()

    mongo.database("foo") must notBeNull
  }
  "return the same database for the same name" in{
    val mongo = new MockMongo()

    mongo.database("foo") must be (mongo.database("foo"))
  }
}