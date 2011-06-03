package blueeyes.persistence.mongo

import org.specs.{Specification, ScalaCheck}

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop._

import MongoFilterOperators._
import blueeyes.json._
import JsonAST._

class MongoFilterSpec extends Specification with ScalaCheck with MongoImplicits with ArbitraryJValue{



//  }
//  "MongoOrFilter" should{
//    "convert to correct JObject" in{
//      forAll { filters: Set[MongoFieldFilter] =>
//        val anotherOrderValues = filters.sortBy(_.lhs)
//        MongoOrFilter(filters).filter == JObject(JField($or.symbol, JArray(filters.map(_.filter))) :: Nil)
//      } must pass
//    }
//  }
}