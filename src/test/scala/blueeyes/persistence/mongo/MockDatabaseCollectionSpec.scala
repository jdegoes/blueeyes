package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST.{JString, JField, JObject}
import blueeyes.json.JPathImplicits._
import MongoFilterOperators._

class MockDatabaseCollectionSpec extends Specification{
  private val jObject  = JObject(JField("address", JObject( JField("city", JString("A")) :: JField("street", JString("1")) ::  Nil)) :: Nil)
  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("2")) ::  Nil)) :: Nil)
  private val jObject2 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("3")) ::  Nil)) :: Nil)
  private val jObject3 = JObject(JField("address", JObject( JField("city", JString("C")) :: JField("street", JString("4")) ::  Nil)) :: Nil)
  private val jobjects = jObject :: jObject1 :: jObject2 :: jObject3 :: Nil

  private val sort     = MongoSort("address.street", MongoSortOrderDescending)

  "store jobjects" in{
    val collection = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jobjects)
  }
  "removes all jobjects when filter is not specified" in{
    val collection = newCollection

    collection.insert(jobjects)
    collection.remove(None) mustEqual(4)
    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(Nil)
  }
  "removes jobjects which match filter" in{
    import MongoFilterImplicits._
    val collection = newCollection

    collection.insert(jobjects)
    collection.remove(Some(MongoFieldFilter("address.city", $eq,"A"))) mustEqual(1)
    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jObject1 :: jObject2 :: jObject3 :: Nil)
  }
  "select all jobjects when filter is not specified" in{
    import MongoFilterImplicits._
    val collection = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jobjects)
  }
  "select jobjects by filter" in{
    import MongoFilterImplicits._
    val collection = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Nil), Some(MongoFieldFilter("address.city", $eq,"A")), None, None, None) mustEqual(jObject :: Nil)
  }
  "select ordered objects" in{
    import MongoFilterImplicits._
    val collection  = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Nil), None, Some(sort), None, None) mustEqual(jobjects.reverse)
  }
  "skip objects" in{
    import MongoFilterImplicits._
    val collection  = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Nil), None, Some(sort), Some(2), None) mustEqual(jObject1 :: jObject:: Nil)
  }
  "limit objects" in{
    import MongoFilterImplicits._
    val collection  = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Nil), None, Some(sort), Some(2), Some(1)) mustEqual(jObject1:: Nil)
  }

  private def newCollection = new MockDatabaseCollection()
}