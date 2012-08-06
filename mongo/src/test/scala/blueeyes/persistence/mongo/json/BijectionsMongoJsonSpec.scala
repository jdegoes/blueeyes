package blueeyes.persistence.mongo.json

import java.util.ArrayList
import blueeyes.json.JsonAST._
import BijectionsMongoJson._
import BijectionsMongoJson.MongoToJson._
import com.mongodb.{BasicDBList, BasicDBObject, DBObject}
import org.specs2.mutable.Specification

class BijectionsMongoJsonSpec extends Specification {
  "MongoToJValue" should{
    "convert string type" in {
      toJson("string", "foo") mustEqual (JString("foo"))
    }
    "convert int type" in {
      toJson("int", 22) mustEqual (JNum(22))
    }
    "convert Double type" in {
      toJson("Double", 22.2) mustEqual (JNum(22.2))
    }
    "convert Long type" in {
      toJson("Double", 1261597307000l) mustEqual (JNum(1261597307000l))
    }
    "convert Float type" in {
      toJson("Float", 22F) mustEqual (JNum(22))
    }
    "convert Boolean type" in {
      toJson("Boolean", true) mustEqual (JBool(true))
    }
    "convert null type" in {
      toJson("null", null) mustEqual (JNull)
    }
    "convert nested DBObject type" in {
      val dbObject = new BasicDBObject()
      dbObject.put("nested", "value")

      toJson("DBObject", dbObject) mustEqual (JObject(List(JField("nested", JString("value")))))
    }
    "convert array type" in {
      val array = new java.util.ArrayList[String]()
      array.add("1")
      array.add("2")
      toJson("array", array) mustEqual (JArray(List(JString("1"), JString("2"))))
    }
    "convert BasicDBList type" in {
      val array = new BasicDBList()
      array.add("1")
      array.add("2")
      toJson("array", array) mustEqual(JArray(List(JString("1"), JString("2"))))
    }
    "convert BasicDBList type" in {
      val array = new BasicDBList()
      array.add(new java.lang.Integer(1))
      array.add("2")
      toJson("array", array) mustEqual(JArray(List(JNum(1), JString("2"))))
    }
//    "remove reserved mongo keys" in {
//      val dbObject = new BasicDBObject()
//      dbObject.put("_id", "4b7d91799790c34331062bc0")
//      val jObject: JObject  = MongoToJson(dbObject) ||| { errors => sys.error(errors.list.mkString("; ")) }
//
//      (jObject \ "_id") mustEqual (JNothing)
//    }
  }
  "JValueToMongo" should {
    "convert string type" in {
      toMongo("string", JString("foo")) mustEqual ("foo")
    }
    "convert int type" in {
      toMongo("int", JNum(22)) mustEqual (22)
    }
    "convert Double type" in {
      toMongo("Double", JNum(22.2)) mustEqual (22.2)
    }
    "convert Long type" in {
      toMongo("Long", JNum(1261597307000l)) mustEqual (1261597307000l)
    }
    "convert Boolean type" in {
      toMongo("Boolean", JBool(true)) mustEqual (java.lang.Boolean.TRUE)
    }
    "convert null type" in {
      toMongo("null", JNull) must be (null)
    }
    "convert JNothing type" in {
      val jObject = toMongoObject("nothing", JNothing)
      jObject.containsField("nothing") mustEqual (false)
    }
    "convert nested jObject type" in {
      val jObject = JObject(List(JField("nested", JString("value"))))

      val dbObject = new BasicDBObject()
      dbObject.put("nested", "value")

      toMongo("jObject", jObject) mustEqual (dbObject)
    }
    "convert nested jField type" in {
      val jObject = JObject(List(JField("jField", JObject(List(JField("nested", JString("value")))))))

      val nestedObject = new BasicDBObject()
      nestedObject.put("nested", "value")

      val dbObject = new BasicDBObject()
      dbObject.put("jField", nestedObject)

      toMongo("jField", jObject) mustEqual (dbObject)
    }
    "convert array type" in {
      val jArray = JArray(List(JString("1"), JString("2")))

      val array = new java.util.ArrayList[String]()
      array.add("1")
      array.add("2")
      toMongo("array", jArray) mustEqual (array)
    }
  }

  private def toJson[T](key: String, value: T) = {
    val dbObject = new BasicDBObject()
    dbObject.put(key, value)    
    jValue(key, MongoToJson(dbObject) ||| { errors => sys.error(errors.list.mkString("; ")) })
  }

  private def toMongo(key: String, value: JValue) = toMongoObject(key, value).get(key)

  private implicit def toMongoObject(key: String, value: JValue): DBObject = {
    MongoToJson.unapply(JObject(List(JField(key, value)))) ||| {
      errors => sys.error(errors.list.mkString("; "))
    }
  }

  private def jValue(key: String, jObject: JObject) = jObject \ key
}
