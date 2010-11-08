package blueeyes.persistence.mongo

import org.specs.Specification
import MongoFilterOperators._
import blueeyes.json.JsonAST._
import blueeyes.json.JPathImplicits._
import blueeyes.json._

class MongoElementsMatchFilterSpec extends Specification{
  private val filter1    = MongoFilterBuilder(JPath("foo")).>(MongoFilterImplicits.MongoPrimitiveInt(1))
  private val filter2    = MongoFilterBuilder(JPath("bar")).<(MongoFilterImplicits.MongoPrimitiveInt(5))
  private val andFilter  = filter1 && filter2

  "create valid json for or filter" in {
    (andFilter.elemMatch("name")).filter mustEqual (JsonParser.parse(""" {"name": {"$elemMatch" : {"foo": {"$gt": 1}, "bar": {"$lt": 5}} }} """))
  }
  "create valid json for or filter when path is empty" in {
    (andFilter.elemMatch("")).filter mustEqual (JsonParser.parse(""" {"$elemMatch" : {"foo": {"$gt": 1}, "bar": {"$lt": 5}} } """))
  }
}