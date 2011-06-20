package blueeyes.persistence.mongo

import org.specs.Specification
import MongoFilterOperators._
import blueeyes.json.JPath
import blueeyes.json.JsonAST._
import MongoFilterImplicits._

class MongoFilterBuilderSpec extends Specification{

  "builds $eq operation" in {
    JPath("foo") === "bar" mustEqual (MongoFieldFilter("foo", $eq, MongoPrimitiveString("bar")))
  }
  "builds $ne operation" in {
    (JPath("foo") !== 1) mustEqual (MongoFieldFilter("foo", $ne, MongoPrimitiveInt(1)))
  }
  "builds $gt operation" in {
    JPath("foo") > 1l mustEqual (MongoFieldFilter("foo", $gt, MongoPrimitiveLong(1l)))
  }
  "builds $gte operation" in {
    JPath("foo") >= 1.1 mustEqual (MongoFieldFilter("foo", $gte, MongoPrimitiveDouble(1.1)))
  }
  "builds $lte operation" in {
    JPath("foo") <= 1.1 mustEqual (MongoFieldFilter("foo", $lte, MongoPrimitiveDouble(1.1)))
  }
  "builds $in operation" in {
    JPath("foo").anyOf(MongoPrimitiveString("foo"), MongoPrimitiveString("bar")) mustEqual (MongoFieldFilter("foo", $in, MongoPrimitiveArray(MongoPrimitiveString("foo") :: MongoPrimitiveString("bar") :: Nil)))
  }
  "builds $all operation" in {
    JPath("foo").contains(MongoPrimitiveString("foo"), MongoPrimitiveString("bar")) mustEqual (MongoFieldFilter("foo", $all, MongoPrimitiveArray(MongoPrimitiveString("foo") :: MongoPrimitiveString("bar") :: Nil)))
  }

  "builds $size operation" in {
    JPath("foo").hasSize(1) mustEqual (MongoFieldFilter("foo", $size, MongoPrimitiveInt(1)))
  }
  "builds $exists operation" in {
    MongoFilterBuilder(JPath("foo")).isDefined mustEqual (MongoFieldFilter("foo", $exists, MongoPrimitiveBoolean(true)))
  }
  "builds $hasType operation" in {
    JPath("foo").hasType[JString] mustEqual (MongoFieldFilter("foo", $type, MongoPrimitiveInt(2)))
  }
  "builds $regex operation" in {
    ("foo" regex "bar") mustEqual (MongoFieldFilter("foo", $regex, MongoPrimitiveJObject(JObject(List(JField("$regex", JString("bar")), JField("$options", JString("")))))))
  }
  "builds $regex operation with options" in {
    ("foo" regex ("bar", "i")) mustEqual (MongoFieldFilter("foo", $regex, MongoPrimitiveJObject(JObject(List(JField("$regex", JString("bar")), JField("$options", JString("i")))))))
  }
}
