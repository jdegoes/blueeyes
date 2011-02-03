package blueeyes.persistence.mongo.mock

import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo.MongoImplicits._
import org.specs.Specification

class MockMongoSpec extends Specification{
  "create database" in{
    val mongo = new MockMongo()

    mongo.database("foo") must notBeNull
  }

  "return the same database for the same name" in{
    val mongo = new MockMongo()

    mongo.database("foo") must be (mongo.database("foo"))
  }

  "Upsert group and select should return the same object" in {
    val mongo = new MockMongo()
    val database = mongo.database("testUpsert")
    val collection = "test"

    val testObj = JObject(JField("properties.groupId", JString("foo")) :: Nil)
    database[JNothing.type](upsert(collection).set(testObj).where("groupId" === "foo"))
    val result = database(selectOne().from(collection))
    result.get must beEqual(testObj)
  }
}
