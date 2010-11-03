package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST.{JString, JField, JObject}
import blueeyes.json.JPathImplicits._
import MongoFilterOperators._

class MockDatabaseCollectionSpec extends Specification{
  val jobject    = JObject(JField("foo", JString("bar")) :: Nil)
  val jobject2   = JObject(JField("name", JString("joe")) :: Nil)

  "store jobjects" in{
    val collection = new MockDatabaseCollection()

    collection.insert(jobject :: jobject2 :: Nil)
    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jobject :: jobject2 :: Nil)
  }
  "removes all jobjects when filter is not specified" in{
    val collection = new MockDatabaseCollection()

    collection.insert(jobject :: jobject2 :: Nil)
    collection.remove(None) mustEqual(2)
    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(Nil)
  }
  "removes jobjects which match filter" in{
    import MongoFilterImplicits._
    val collection = new MockDatabaseCollection()

    collection.insert(jobject :: jobject2 :: Nil)
    collection.remove(Some(MongoFieldFilter("foo", $eq,"bar"))) mustEqual(1)
    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jobject2 :: Nil)
  }
  "select all jobjects when filter is not specified" in{
    import MongoFilterImplicits._
    val collection = new MockDatabaseCollection()

    collection.insert(jobject :: jobject2 :: Nil)
    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jobject :: jobject2 :: Nil)
  }
  "select jobjects by filter" in{
    import MongoFilterImplicits._
    val collection = new MockDatabaseCollection()

    collection.insert(jobject :: jobject2 :: Nil)
    collection.select(MongoSelection(Nil), Some(MongoFieldFilter("foo", $eq,"bar")), None, None, None) mustEqual(jobject :: Nil)
  }
}