package blueeyes.persistence.mongo.mock

import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo.MongoImplicits._
import org.specs2.mutable.Specification
import org.specs2.matcher.MustThrownMatchers

class MockMongoSpec extends Specification with MustThrownMatchers{
  "create database" in{
    val mongo = new MockMongo()

    mongo.database("foo") must not beNull
  }

  "return the same database for the same name" in{
    val mongo = new MockMongo()

    mongo.database("foo") must be (mongo.database("foo"))
  }
}
