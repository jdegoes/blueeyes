package blueeyes.persistence.mongo

import org.specs.Specification
import MongoQueryOperators._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JPath

class MongoQueryBuilderSpec extends Specification{

  "builds $eq operation" in {
    import MongoQueryImplicits._    
    MongoQueryBuilder(JPath("foo"))===("bar") mustEqual (MongoFieldQuery("foo", $eq, "bar"))
  }
  "builds $ne operation" in {
    import MongoQueryImplicits._
    MongoQueryBuilder(JPath("foo")).!==(1) mustEqual (MongoFieldQuery("foo", $ne, 1))
  }
  "builds $gt operation" in {
    import MongoQueryImplicits._
    MongoQueryBuilder(JPath("foo")).>(11) mustEqual (MongoFieldQuery("foo", $gt, 11))
  }
  "builds $gte operation" in {
    import MongoQueryImplicits._
    MongoQueryBuilder(JPath("foo")).>=(1.1) mustEqual (MongoFieldQuery("foo", $gte, 1.1))
  }
  "builds $in operation" in {
    import MongoQueryImplicits._
    MongoQueryBuilder(JPath("foo")).in(MongoPrimitiveString("foo"), MongoPrimitiveString("bar")) mustEqual (MongoFieldQuery("foo", $in, MongoPrimitiveString("foo") :: MongoPrimitiveString("bar") :: Nil))
  }
  "builds $all operation" in {
    import MongoQueryImplicits._
    MongoQueryBuilder(JPath("foo")).contains(MongoPrimitiveString("foo"), MongoPrimitiveString("bar")) mustEqual (MongoFieldQuery("foo", $all, MongoPrimitiveString("foo") :: MongoPrimitiveString("bar") :: Nil))
  }
}
