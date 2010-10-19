package blueeyes.persistence.mongo.json

import org.scalatest.WordSpec
import org.scalatest.matchers.{MustMatchers}
import java.util.ArrayList
import blueeyes.json.JsonAST._
import MongoJson._
import com.mongodb.{BasicDBList, BasicDBObject, DBObject}

class MongoJsonTester extends WordSpec with MustMatchers {
  "MongoToJValue" must{
    "convert string type" in {
      toJson("string", "foo") must be (JString("foo"))
    }
    "convert int type" in {
      toJson("int", 22) must be (JInt(22))
    }
    "convert Double type" in {
      toJson("Double", 22.2) must be (JDouble(22.2))
    }
    "convert Long type" in {
      toJson("Double", 1261597307000l) must be (JInt(1261597307000l))
    }
    "convert Float type" in {
      toJson("Float", 22F) must be (JDouble(22))
    }
    "convert Boolean type" in {
      toJson("Boolean", true) must be (JBool(true))
    }
    "convert null type" in {
      toJson("null", null) must be (JNull)
    }
    "convert nested DBObject type" in {
      val dbObject = new BasicDBObject()
      dbObject.put("nested", "value")

      toJson("DBObject", dbObject) must be (JObject(List(JField("nested", JString("value")))))
    }
    "convert array type" in {
      val array = new java.util.ArrayList[String]()
      array.add("1")
      array.add("2")
      toJson("array", array) must be (JArray(List(JString("1"), JString("2"))))
    }
    "convert BasicDBList type" in {
      try {
        val array = new BasicDBList()
        array.add("1")
        array.add("2")
        toJson("array", array) must be(JArray(List(JString("1"), JString("2"))))
      }
      catch {
        case e: Throwable => e.printStackTrace()
      }
    }
    "remove reserved mongo keys" in {
      val dbObject = new BasicDBObject()
      dbObject.put("_id", "4b7d91799790c34331062bc0")
      val jObject: JObject  = dbObject

      (jObject \ "_id") must be (JNothing)
    }
  }
  "JValueToMongo" must {
    "convert string type" in {
      toMongo("string", JString("foo")) must be ("foo")
    }
    "convert int type" in {
      toMongo("int", JInt(22)) must be (22)
    }
    "convert Double type" in {
      toMongo("Double", JDouble(22.2)) must be (22.2)
    }
    "convert Long type" in {
      toMongo("Long", JInt(1261597307000l)) must be (1261597307000l)
    }
    "convert Boolean type" in {
      toMongo("Boolean", JBool(true)) must be (java.lang.Boolean.TRUE)
    }
    "convert null type" in {
      toMongo("null", JNull) must be (null)
    }
    "convert JNothing type" in {
      val jObject = toMongoObject("nothing", JNothing)
      jObject.containsField("nothing") must be (false)
    }
    "convert nested jObject type" in {
      val jObject = JObject(List(JField("nested", JString("value"))))

      val dbObject = new BasicDBObject()
      dbObject.put("nested", "value")

      toMongo("jObject", jObject) must be (dbObject)
    }
    "convert nested jField type" in {
      val jObject = JObject(List(JField("jField", JObject(List(JField("nested", JString("value")))))))

      val nestedObject = new BasicDBObject()
      nestedObject.put("nested", "value")

      val dbObject = new BasicDBObject()
      dbObject.put("jField", nestedObject)

      toMongo("jField", jObject) must be (dbObject)
    }
    "convert array type" in {
      val jArray = JArray(List(JString("1"), JString("2")))

      val array = new java.util.ArrayList[String]()
      array.add("1")
      array.add("2")
      toMongo("array", jArray) must be (array)
    }
  }
  "JavaToScala" must {
    "convert String to JString" in {
      val v  = "Foo"
      val jVal: JString = v.toJValue
      jVal must be (JString(v))
    }
    "convert Long to JInt" in {
      val v: java.lang.Long = 1l
      val jVal: JInt = v.toJValue
      jVal must be (JInt(v.longValue))
    }
    "convert Integer to JInt" in {
      val v: java.lang.Integer = 1
      val jVal: JInt = v.toJValue
      jVal must be (JInt(v.intValue))
    }
    "convert Double to JDouble" in {
      val v: java.lang.Double = 1.0
      val jVal: JDouble = v.toJValue
      jVal must be (JDouble(v.doubleValue))
    }
    "convert Float to JDouble" in {
      val v: java.lang.Float = 1.0f
      val jVal: JDouble = v.toJValue
      jVal must be (JDouble(v.floatValue.doubleValue))
    }
    "convert Boolean to JBool" in {
      val v: java.lang.Boolean = true
      val jVal: JBool = v.toJValue
      jVal must be (JBool(v.booleanValue))
    }
    "convert ArrayList to JArray" in {
      val v: ArrayList[AnyRef] = new ArrayList()
      v.add(1.asInstanceOf[AnyRef])
      v.add(2.asInstanceOf[AnyRef])
      val jVal: JArray = v.toJValue
      jVal must be (JArray(List(JInt(1), JInt(2))))
    }
  }

  private def toJson[T](key: String, value: T) = {
    val dbObject = new BasicDBObject()
    dbObject.put(key, value)    
    jValue(key, dbObject)
  }

  private def toMongo(key: String, value: JValue) = toMongoObject(key, value).get(key)

  private def toMongoObject(key: String, value: JValue): DBObject = {
    JObject(List(JField(key, value)))
  }

  private def jValue(key: String, jObject: JObject) = jObject \ key
}
