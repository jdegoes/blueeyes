package blueeyes.persistence.mongo

import org.specs.Specification
import MongoFilterOperators._
import blueeyes.json.JsonAST._
import blueeyes.json._

class MongoAndFilterSpec extends Specification{
  private val filter1    = MongoFilterBuilder(JPath("foo")).>(MongoFilterImplicits.MongoPrimitiveInt(1))
  private val filter2    = MongoFilterBuilder(JPath("bar")).<(MongoFilterImplicits.MongoPrimitiveInt(5))
  private val filter3    = MongoFilterBuilder(JPath("rar")).<(MongoFilterImplicits.MongoPrimitiveInt(6))
  private val andFilter  = filter1 && filter2

  "create valid json for or filter" in {
    (andFilter).filter mustEqual (JObject(filter1.filter.asInstanceOf[JObject].fields ++ filter2.filter.asInstanceOf[JObject].fields))
  }

  "combine with filter3 to create a new filter" in {
    (andFilter && filter3).filter mustEqual (JObject(filter1.filter.asInstanceOf[JObject].fields ++ 
      filter2.filter.asInstanceOf[JObject].fields ++ 
      filter3.filter.asInstanceOf[JObject].fields))
  }

  "combine ANDs with ORs" in {
    import MongoFilterImplicits._
    val exam: MongoFilter = ("address.city" === "B") ||  ("address.street" === "2") || ("address.code" === 1)
    val cfilter: MongoFilter = ((filter1 && filter2 && filter3) || (filter2 && filter3) || (filter1 && filter3)) //|| (filter1 && filter2)
    cfilter.filter mustEqual
    JsonParser.parse("""
       {
      "$or":[{
        "foo":{
          "$gt":1
        },
        "bar":{
          "$lt":5
        },
        "rar":{
          "$lt":6
        }
      },{
        "bar":{
          "$lt":5
        },
        "rar":{
          "$lt":6
        }
      },{
        "foo":{
          "$gt":1
        },
        "rar":{
          "$lt":6
        }
      }]
    }
  """)
  }

  "unary_! use 'or' use with negative operators of subfilters " in{
    (andFilter).unary_! mustEqual (filter1.unary_! || filter2.unary_!)
  }

  "2 unary_! result to the same filter" in{
    (andFilter).unary_!.unary_! mustEqual (andFilter)
  }

  "and with MongoFilterAll return filter" in{
    (MongoFilterAll && filter2).filter mustEqual (filter2.filter)
    (filter2 && MongoFilterAll).filter mustEqual (filter2.filter)
  }
  
}
