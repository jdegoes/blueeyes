package blueeyes.persistence.mongo

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop._

import MongoFilterOperators._
import blueeyes.json.JsonAST._
import blueeyes.json._
import MongoFilterImplicits._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

class MongoOrFilterSpec extends Specification with ScalaCheck with MongoImplicits with ArbitraryJValue with ArbitraryMongo{
  private val filter1  = MongoFilterBuilder(JPath("foo")).>(MongoPrimitiveInt(1))
  private val filter2  = MongoFilterBuilder(JPath("bar")).<(MongoPrimitiveInt(5))
  private val filter3  = MongoFilterBuilder(JPath("baz")).>(MongoPrimitiveInt(1))
  private val filter4  = MongoFilterBuilder(JPath("foz")).<(MongoPrimitiveInt(5))
  private val orFilter = filter1 || filter2

  def getDifferentOrdersOrs: Gen[(MongoOrFilter, MongoOrFilter)] = getListMongoFieldFilter.map{filters =>
    def orFilter(values: List[MongoFieldFilter]) = values.tail.foldLeft(MongoOrFilter(List(values.head))){(andFilter, filter) => (andFilter || filter).asInstanceOf[MongoOrFilter]}
    (orFilter(filters), orFilter(filters.reverse))
  }

  implicit def arbDifferentOrdersOrs: Arbitrary[(MongoOrFilter, MongoOrFilter)] = Arbitrary(getDifferentOrdersOrs)

  "MongoOrFilter" should{
    "convert to the same JValue, no matter the order of constructions" in{
      check { filters: (MongoOrFilter, MongoOrFilter) =>
        def orValue(filter: MongoOrFilter) = filter.filter.sort

        orValue(filters._1) == orValue(filters._2)
      }
    }
    "should equal, no matter the order of constructions" in{
      check { filters: (MongoOrFilter, MongoOrFilter) => filters._1 == filters._2 }
    }
    "should have the same hashCodes, no matter the order of constructions" in{
      check { filters: (MongoOrFilter, MongoOrFilter) => filters._1.hashCode == filters._1.hashCode }
    }

    "create valid json for or filter" in {
      (orFilter).filter mustEqual (JObject(JField("$or", JArray(filter1.filter :: filter2.filter :: Nil)) :: Nil))
      (orFilter | (filter3 | filter4)).filter mustEqual (JObject(JField("$or", JArray(filter1.filter :: filter2.filter :: filter3.filter :: filter4.filter :: Nil)) :: Nil))
      (orFilter | filter3).filter mustEqual (JObject(JField("$or", JArray(filter1.filter :: filter2.filter :: filter3.filter :: Nil)) :: Nil))
      (filter3 | orFilter).filter mustEqual (JObject(JField("$or", JArray(filter3.filter :: filter1.filter :: filter2.filter :: Nil)) :: Nil))
    }
    "unary_! use 'and' use with negative operators of subfilters " in{
      (orFilter).unary_! mustEqual (filter1.unary_! && filter2.unary_!)
    }
    "several or does not create recursion" in{
      ("address.city" === "B") || ( ("address.street" === "2") || ("address.code" === 1) ) mustEqual (MongoOrFilter(List(("address.city" === "B"), ("address.street" === "2"), ("address.code" === 1))))
      ("address.city" === "B") || ("address.street" === "2") || ("address.code" === 1) mustEqual (MongoOrFilter(List(("address.city" === "B"), ("address.street" === "2"), ("address.code" === 1))))
    }

    "2 unary_! results to the same filter" in{
      (orFilter).unary_!.unary_! mustEqual (orFilter)
    }
    "or with MongoFilterAll return filter" in{
      MongoFilterAll || filter1 mustEqual (filter1)
      filter1 || MongoFilterAll mustEqual (filter1)
    }
  }
}
