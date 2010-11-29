package blueeyes.persistence.mongo

import org.spex.Specification
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import MongoUpdateOperators._
import MongoFilterOperators._
import blueeyes.json.JsonAST._
import blueeyes.json.{JsonParser, JPath, Printer}

//class MongoUpdateObjectSpec extends Specification{
//  private val jObject  = JObject(JField("address", JObject( JField("city", JString("A")) :: JField("street", JString("1")) ::  Nil)) :: Nil)
//  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("2")) ::  Nil)) :: Nil)

//  "build valid json with 'AND' MongoUpdateFieldValue" in {
//    import MongoFilterImplicits._
//    val updateObject = MongoUpdateObject(jObject)
//    val updateFields = ("x" inc (1)) & ("address" set (jvalueToMongoPrimitive(jObject.fields.head.value).get))
//    val andUpdate    = ("x" inc (1)) & updateObject
//
//    andUpdate.toJValue mustEqual (updateFields.toJValue)
//  }

//}