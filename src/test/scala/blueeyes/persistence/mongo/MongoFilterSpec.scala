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

  def getMongoFieldFilter[Gen[MongoFieldFilter]] = for{
    path     <- arbitrary[String].map(JPath(_))
    operator <- Gen.oneOf($eq, $ne, $gt, $gte, $lt, $lte, $in, $all, $size, $exists, $type)
    value    <- genSimple
  } yield(MongoFieldFilter(path, operator, value))

  def getListMongoFieldFilter = Gen.containerOfN[List, MongoFieldFilter](choose(2, 8).sample.get, getMongoFieldFilter)

  implicit def arbListMongoFieldFilter: Arbitrary[List[MongoFieldFilter]] = Arbitrary(getListMongoFieldFilter)

  "MongoAndFilter" should{
    "convert to the same JValue, no matter the order of constructions" in{
      forAll { filters: List[MongoFieldFilter] =>
        val anotherOrderValues = filters.sortBy(_.lhs)
        MongoAndFilter(filters).filter == MongoAndFilter(anotherOrderValues).filter
      } must pass
    }
  }
  "MongoOrFilter" should{
    "convert to correct JObject" in{
      forAll { filters: List[MongoFieldFilter] =>
        val anotherOrderValues = filters.sortBy(_.lhs)
        MongoOrFilter(filters).filter == JObject(JField($or.symbol, JArray(filters.map(_.filter))) :: Nil)
      } must pass
    }
  }
}