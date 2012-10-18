package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import MongoFilterOperators._
import blueeyes.json._
import blueeyes.json.JPathImplicits._
import blueeyes.json._

class MongoElementsMatchFilterSpec extends Specification{
  private val filter1    = MongoFilterBuilder(JPath("foo")).>(MongoPrimitiveInt(1))
  private val filter2    = MongoFilterBuilder(JPath("bar")).<(MongoPrimitiveInt(5))
  private val andFilter  = filter1 && filter2

  "create valid json for or filter" in {
    (andFilter.elemMatch("name")).filter mustEqual (JParser.parse(""" {"name": {"$elemMatch" : {"foo": {"$gt": 1}, "bar": {"$lt": 5}} }} """))
  }
  "create valid json for or filter when path is empty" in {
    (andFilter.elemMatch("")).filter mustEqual (JParser.parse(""" {"$elemMatch" : {"foo": {"$gt": 1}, "bar": {"$lt": 5}} } """))
  }
}