package blueeyes.persistence.mongo

import org.specs.Specification
import MongoFilterOperators._
import blueeyes.json.JsonAST._
import blueeyes.json._

class MongoOrFilterSpec extends Specification{
  private val filter1  = MongoFilterBuilder(JPath("foo")).>(MongoFilterImplicits.MongoPrimitiveInt(1))
  private val filter2  = MongoFilterBuilder(JPath("bar")).<(MongoFilterImplicits.MongoPrimitiveInt(5))
  private val orFilter = filter1 || filter2
  
  "create valid json for or filter" in {
    (orFilter).filter mustEqual (JObject(JField("$or", JArray(filter1.filter :: filter2.filter :: Nil)) :: Nil))
  }
  "unary_! use 'and' use with negative operators of subfilters " in{
    (orFilter).unary_! mustEqual (filter1.unary_! && filter2.unary_!)
  }
  "several or does not create recursion" in{
    import MongoFilterImplicits._
    ("address.city" === "B") || ( ("address.street" === "2") || ("address.code" === 1) ) mustEqual (MongoOrFilter(("address.city" === "B") :: ("address.street" === "2") :: ("address.code" === 1) :: Nil))
    ("address.city" === "B") || ("address.street" === "2") || ("address.code" === 1) mustEqual (MongoOrFilter(("address.city" === "B") :: ("address.street" === "2") :: ("address.code" === 1) :: Nil))
  }

  "2 unary_! results to the same filter" in{
    (orFilter).unary_!.unary_! mustEqual (orFilter)
  }
  "or with MongoFilterAll return filter" in{
    MongoFilterAll || filter1 mustEqual (filter1)
    filter1 || MongoFilterAll mustEqual (filter1)
  }
}
