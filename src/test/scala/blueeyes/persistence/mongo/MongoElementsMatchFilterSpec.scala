package blueeyes.persistence.mongo

import org.specs.Specification
import MongoFilterOperators._
import blueeyes.json.JsonAST._
import blueeyes.json._

class MongoElementsMatchFilterSpec /*extends Specification{
  private val filter1    = MongoFilterBuilder(JPath("foo")).>(MongoFilterImplicits.MongoPrimitiveInt(1))
  private val filter2    = MongoFilterBuilder(JPath("bar")).<(MongoFilterImplicits.MongoPrimitiveInt(5))
  private val andFilter  = filter1 && filter2

  "create valid json for or filter" in {
    (andFilter).filter mustEqual (JObject(filter1.filter.fields ++ filter2.filter.fields))
  }
  "unary_! use 'or' use with negative operators of subfilters " in{
    (andFilter).unary_! mustEqual (filter1.unary_! || filter2.unary_!)
  }

  "2 unary_! result to the same filter" in{
    (andFilter).unary_!.unary_! mustEqual (andFilter)
  }
}*/