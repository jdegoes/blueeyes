package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import MongoFilterOperators._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JPath

class JObjectsFilterSpec extends Specification{
  private val jObject  = JObject(JField("address", JObject( JField("city", JString("A")) :: JField("street", JString("1")) ::  Nil)) :: Nil)
  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("2")) ::  Nil)) :: Nil)
  private val jObject2 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("3")) ::  Nil)) :: Nil)
  private val jObject3 = JObject(JField("address", JObject( JField("city", JString("C")) :: JField("street", JString("4")) ::  Nil)) :: Nil)
  private val jobjects = jObject :: jObject1 :: jObject2 :: jObject3 :: Nil

  "selects objects by field query" in{
    import MongoFilterImplicits._
    JObjectsFilter(jobjects, MongoFieldFilter("address.city", $eq,"B")) mustEqual(jObject1 :: jObject2 :: Nil)
  }
  "selects objects by 'or' query" in{
    import MongoFilterImplicits._
    val result = jObject1 :: jObject2 :: jObject3 :: Nil
    JObjectsFilter(jobjects, MongoFieldFilter("address.city", $eq,"B") || MongoFieldFilter("street.street", $eq,"4")).filterNot (result contains) mustEqual(Nil)
  }

}