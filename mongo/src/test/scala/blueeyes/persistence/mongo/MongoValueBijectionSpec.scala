package blueeyes.persistence.mongo

import BijectionsMongoValue._
import blueeyes.json._
import org.specs2.mutable.Specification
import org.joda.time.DateTime
import java.util.regex.Pattern
import java.util.regex.Pattern._
import com.mongodb.{DBObject, BasicDBList, BasicDBObject}

class MongoValueBijectionSpec extends Specification {
  "fromDBObject" should{
    "convert string type" in {
      testFromMongoObject("string", "foo", MongoString("foo"))
    }
    "convert int type" in {
      testFromMongoObject("int", 22, MongoInt(22))
    }
    "convert Double type" in {
      testFromMongoObject("Double", 22.2, MongoDouble(22.2))
    }
    "convert Binary type" in {
      val converted = fromMongoObject("Binary", "ab".getBytes)
      converted.fields.size mustEqual(1)
      converted.fields.head.value.asInstanceOf[MongoBinary].value.toList mustEqual(List[Byte]('a', 'b'))
    }
    "convert Date type" in {
      val value = new java.util.Date()
      testFromMongoObject("Date", value, MongoDate(new DateTime(value)))
    }
    "convert Regex type" in {
      testFromMongoObject("Regex", Pattern.compile("joh?n", CASE_INSENSITIVE), MongoRegex("joh?n", Some(CASE_INSENSITIVE)))
    }
    "convert Long type" in {
      testFromMongoObject("Long", 1261597307000l, MongoLong(1261597307000l))
    }
    "convert Float type" in {
      testFromMongoObject("Float", 22F, MongoFloat(22F))
    }
    "convert Boolean type" in {
      testFromMongoObject("Boolean", true, MongoBoolean(true))
    }
    "convert null type" in {
      testFromMongoObject("null", null, MongoNull)
    }
    "convert nested DBObject type" in {
      val dbObject = new BasicDBObject()
      dbObject.put("nested", "value")

      testFromMongoObject("DBObject", dbObject, MongoObject(List(MongoField("nested", MongoString("value")))))
    }
    "convert array type" in {
      val array = new java.util.ArrayList[String]()
      array.add("1")
      array.add("2")
      testFromMongoObject("array", array, MongoArray(List(MongoString("1"), MongoString("2"))))
    }
    "convert BasicDBList type" in {
      val array = new BasicDBList()
      array.add("1")
      array.add("2")
      testFromMongoObject("array", array, MongoArray(List(MongoString("1"), MongoString("2"))))
    }
    "convert BasicDBList type" in {
      val array = new BasicDBList()
      array.add(new java.lang.Integer(1))
      array.add("2")
      testFromMongoObject("array", array, MongoArray(List(MongoInt(1), MongoString("2"))))
    }
    //"remove reserved mongo keys" in {
    //  val dbObject = new BasicDBObject()
    //  dbObject.put("_id", "4b7d91799790c34331062bc0")

    //  MongoToMongoValue(dbObject) mustEqual (MongoObject(Nil))
    //}
  }

  private def testFromMongoObject[T](key: String, mongoValue: T, value: MongoValue) = {
    val converted = fromMongoObject(key, mongoValue)
    converted mustEqual (MongoObject(List(MongoField(key, value))))
  }

  private def fromMongoObject[T](key: String, mongoValue: T) = {
    val dbObject = new BasicDBObject()
    dbObject.put(key, mongoValue)

    MongoToMongoValue(dbObject) valueOr {
      errors => sys.error(errors.list.mkString("; "))
    }
  }

  "toDBObject" should {
    "convert string type" in {
      toMongo("string", MongoString("foo")) mustEqual ("foo")
    }
    "convert int type" in {
      toMongo("int", MongoInt(22)) mustEqual (22)
    }
    "convert Double type" in {
      toMongo("Double", MongoDouble(22.2)) mustEqual (22.2)
    }
    "convert Date type" in {
      val dateTime = new DateTime()
      toMongo("Date", MongoDate(dateTime)) mustEqual (dateTime.toDate)
    }
    "convert Regex type" in {
      val dbValue = toMongo("Regex", MongoRegex("joh?n", Some(CASE_INSENSITIVE))).asInstanceOf[Pattern]
      dbValue.pattern mustEqual ("joh?n")
      dbValue.flags mustEqual (CASE_INSENSITIVE)
    }
    "convert Binary type" in {
      toMongo("Binary", MongoBinary("foo".getBytes)).asInstanceOf[Array[Byte]].toList mustEqual (List('f', 'o', 'o'))
    }
    "convert Float type" in {
      toMongo("Float", MongoFloat(22.2F)) mustEqual (22.2F)
    }
    "convert Long type" in {
      toMongo("Long", MongoLong(1261597307000l)) mustEqual (1261597307000l)
    }
    "convert Boolean type" in {
      toMongo("Boolean", MongoBoolean(true)) mustEqual (java.lang.Boolean.TRUE)
    }
    "convert null type" in {
      toMongo("null", MongoNull) must be (null)
    }
    "convert nested Object type" in {
      val mongoObject = MongoObject(List(MongoField("nested", MongoString("value"))))

      val dbObject = new BasicDBObject()
      dbObject.put("nested", "value")

      toMongo("jObject", mongoObject) mustEqual (dbObject)
    }
    "convert nested Field type" in {
      val mongoObject = MongoObject(List(MongoField("field", MongoObject(List(MongoField("nested", MongoString("value")))))))

      val nestedObject = new BasicDBObject()
      nestedObject.put("nested", "value")

      val dbObject = new BasicDBObject()
      dbObject.put("field", nestedObject)

      toMongo("field", mongoObject) mustEqual (dbObject)
    }
    "convert array type" in {
      val mongoArray = MongoArray(List(MongoString("1"), MongoString("2")))

      val array = new java.util.ArrayList[String]()
      array.add("1")
      array.add("2")
      toMongo("array", mongoArray) mustEqual (array)
    }
  }

  private def toMongo(key: String, value: MongoValue) = toMongoObject(key, value).get(key)

  private def toMongoObject(key: String, value: MongoValue): DBObject = MongoValueToMongo(MongoObject(List(MongoField(key, value)))) valueOr {
    errors => sys.error(errors.list.mkString("; "))
  }
}
