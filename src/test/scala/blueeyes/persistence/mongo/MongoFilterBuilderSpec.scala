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
  "builds $near" in {
    ("foo" near (50, 60)) mustEqual (MongoFieldFilter("foo", $near, MongoPrimitiveJObject(JObject(List(JField("$near", JArray(List(JDouble(50.0), JDouble(60.0)))))))))
  }
  "builds $within for box" in {
    ("foo" within Box((10, 20), (30, 40))) mustEqual (MongoFieldFilter("foo", $within, MongoPrimitiveJObject(JObject(JField("$within", JObject(JField("$box", JArray(JArray(JDouble(10.0) :: JDouble(20.0) :: Nil) :: JArray(JDouble(30.0) :: JDouble(40.0) :: Nil) :: Nil)) :: Nil)) :: Nil))))
  }
  "builds $within for circe" in {
    ("foo" within Circle((10, 20), 30)) mustEqual (MongoFieldFilter("foo", $within, MongoPrimitiveJObject(JObject(JField("$within", JObject(JField("$center",  JArray(JArray(JDouble(10.0) :: JDouble(20.0) :: Nil) :: JDouble(30.0) :: Nil)) :: Nil)) :: Nil))))
  }
  "builds $within for CenterSphere" in {
    ("foo" within CenterSphere((10, 20), 0.3)) mustEqual (MongoFieldFilter("foo", $within, MongoPrimitiveJObject(JObject(JField("$within", JObject(JField("$centerSphere",  JArray(JArray(JDouble(10.0) :: JDouble(20.0) :: Nil) :: JDouble(0.3) :: Nil)) :: Nil)) :: Nil))))
  }
  "builds $within for polygon" in {
    ("foo" within Polygon((10, 20), (30, 40))) mustEqual (MongoFieldFilter("foo", $within, MongoPrimitiveJObject(JObject(JField("$within", JObject(JField("$polygon", JArray(JArray(JDouble(10.0) :: JDouble(20.0) :: Nil) :: JArray(JDouble(30.0) :: JDouble(40.0) :: Nil) :: Nil)) :: Nil)) :: Nil))))
  }
  "builds $where operation" in {
    (evaluation("this.a > 3")) mustEqual (MongoFieldFilter(JPath.Identity, $where, MongoPrimitiveString("this.a > 3")))
  }

}
