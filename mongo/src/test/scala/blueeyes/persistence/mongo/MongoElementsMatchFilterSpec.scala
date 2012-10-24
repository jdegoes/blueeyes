package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import MongoFilterOperators._
import blueeyes.json._
import blueeyes.json.JPathImplicits._
import blueeyes.json._

import scalaz.Success

class MongoElementsMatchFilterSpec extends Specification{
  private val filter1    = MongoFilterBuilder(JPath("foo")).>(MongoPrimitiveInt(1))
  private val filter2    = MongoFilterBuilder(JPath("bar")).<(MongoPrimitiveInt(5))
  private val andFilter  = filter1 && filter2

  "create valid json for or filter" in {
    Success((andFilter.elemMatch("name")).filter) mustEqual (JParser.parseFromString(""" {"name": {"$elemMatch" : { "$and" : [{"bar": {"$lt": 5}}, {"foo": {"$gt": 1}}] } }} """))
  }
  "create valid json for or filter when path is empty" in {
    Success((andFilter.elemMatch("")).filter) mustEqual (JParser.parseFromString(""" {"$elemMatch" : { "$and": [{"bar": {"$lt": 5}}, {"foo": {"$gt": 1}}] } } """))
  }
}
