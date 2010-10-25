package blueeyes.persistence.mongo

import org.specs.Specification
import MongoFilterOperators._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JPath
import blueeyes.json.JsonAST.JString

class MongoFilterBuilderSpec extends Specification{

  "builds $eq operation" in {
    import MongoFilterImplicits._
    MongoFilterBuilder(JPath("foo"))===("bar") mustEqual (MongoFieldFilter("foo", $eq, MongoPrimitiveString("bar")))
  }
  "builds $ne operation" in {
    import MongoFilterImplicits._
    MongoFilterBuilder(JPath("foo")).!==(1) mustEqual (MongoFieldFilter("foo", $ne, MongoPrimitiveInt(1)))
  }
  "builds $gt operation" in {
    import MongoFilterImplicits._
    MongoFilterBuilder(JPath("foo")).>(1l) mustEqual (MongoFieldFilter("foo", $gt, MongoPrimitiveLong(1l)))
  }
  "builds $gte operation" in {
    import MongoFilterImplicits._
    MongoFilterBuilder(JPath("foo")).>=(1.1) mustEqual (MongoFieldFilter("foo", $gte, MongoPrimitiveDouble(1.1)))
  }
  "builds $in operation" in {
    import MongoFilterImplicits._
    MongoFilterBuilder(JPath("foo")).in(MongoPrimitiveString("foo"), MongoPrimitiveString("bar")) mustEqual (MongoFieldFilter("foo", $in, MongoPrimitiveArray(MongoPrimitiveString("foo") :: MongoPrimitiveString("bar") :: Nil)))
  }
  "builds $all operation" in {
    import MongoFilterImplicits._
    MongoFilterBuilder(JPath("foo")).contains(MongoPrimitiveString("foo"), MongoPrimitiveString("bar")) mustEqual (MongoFieldFilter("foo", $all, MongoPrimitiveArray(MongoPrimitiveString("foo") :: MongoPrimitiveString("bar") :: Nil)))
  }

  "builds $size operation" in {
    import MongoFilterImplicits._
    MongoFilterBuilder(JPath("foo")).hasSize(1) mustEqual (MongoFieldFilter("foo", $size, MongoPrimitiveInt(1)))
  }
  "builds $exists operation" in {
    import MongoFilterImplicits._
    MongoFilterBuilder(JPath("foo")).exists mustEqual (MongoFieldFilter("foo", $exists, MongoPrimitiveBoolean(true)))
  }
  "builds $hasType operation" in {
    import MongoFilterImplicits._
    MongoFilterBuilder(JPath("foo")).hasType[JString] mustEqual (MongoFieldFilter("foo", $type, MongoPrimitiveInt(2)))
  }
}
