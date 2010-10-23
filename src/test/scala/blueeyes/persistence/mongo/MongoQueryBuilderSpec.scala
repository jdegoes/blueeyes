package blueeyes.persistence.mongo

import org.specs.Specification
import MongoQueryOperators._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JPath
import blueeyes.json.JsonAST.JString

class MongoQueryBuilderSpec extends Specification{

  "builds $eq operation" in {
    import MongoQueryImplicits._    
    MongoQueryBuilder(JPath("foo"))===("bar") mustEqual (MongoFieldQuery("foo", $eq, MongoPrimitiveString("bar")))
  }
  "builds $ne operation" in {
    import MongoQueryImplicits._
    MongoQueryBuilder(JPath("foo")).!==(1) mustEqual (MongoFieldQuery("foo", $ne, MongoPrimitiveInt(1)))
  }
  "builds $gt operation" in {
    import MongoQueryImplicits._
    MongoQueryBuilder(JPath("foo")).>(1l) mustEqual (MongoFieldQuery("foo", $gt, MongoPrimitiveLong(1l)))
  }
  "builds $gte operation" in {
    import MongoQueryImplicits._
    MongoQueryBuilder(JPath("foo")).>=(1.1) mustEqual (MongoFieldQuery("foo", $gte, MongoPrimitiveDouble(1.1)))
  }
  "builds $in operation" in {
    import MongoQueryImplicits._
    MongoQueryBuilder(JPath("foo")).in(MongoPrimitiveString("foo"), MongoPrimitiveString("bar")) mustEqual (MongoFieldQuery("foo", $in, MongoPrimitiveArray(MongoPrimitiveString("foo") :: MongoPrimitiveString("bar") :: Nil)))
  }
  "builds $all operation" in {
    import MongoQueryImplicits._
    MongoQueryBuilder(JPath("foo")).contains(MongoPrimitiveString("foo"), MongoPrimitiveString("bar")) mustEqual (MongoFieldQuery("foo", $all, MongoPrimitiveArray(MongoPrimitiveString("foo") :: MongoPrimitiveString("bar") :: Nil)))
  }

  "builds $size operation" in {
    import MongoQueryImplicits._
    MongoQueryBuilder(JPath("foo")).hasSize(1) mustEqual (MongoFieldQuery("foo", $size, MongoPrimitiveInt(1)))
  }
  "builds $exists operation" in {
    import MongoQueryImplicits._
    MongoQueryBuilder(JPath("foo")).exists mustEqual (MongoFieldQuery("foo", $exists, MongoPrimitiveBoolean(true)))
  }
  "builds $hasType operation" in {
    import MongoQueryImplicits._
    MongoQueryBuilder(JPath("foo")).hasType[JString] mustEqual (MongoFieldQuery("foo", $type, MongoPrimitiveInt(2)))
  }
}
