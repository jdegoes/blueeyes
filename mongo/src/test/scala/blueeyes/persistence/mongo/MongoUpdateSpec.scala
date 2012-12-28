package blueeyes.persistence.mongo

import org.scalacheck.Gen
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop._

import MongoUpdateOperators._
import MongoFilterOperators._
import blueeyes.json._
import blueeyes.json.{ArbitraryJValue}

import scalaz._
import Scalaz._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import dsl._

class MongoUpdateSpec extends Specification with ScalaCheck with ArbitraryJValue with ArbitraryMongo{

  def getDifferentOrdersUpdates: Gen[(MongoUpdate, MongoUpdate)] = getListMongoUpdate.map{updates =>
    def andUpdate(values: List[MongoUpdate]) = {values.foldLeft(MongoUpdateNothing.asInstanceOf[MongoUpdate]){(andUpdate, update) => (andUpdate |+| update)}    }
    (andUpdate(updates), andUpdate(updates.reverse))
  }

  implicit def arbDifferentOrdersAnds: Arbitrary[(MongoUpdate, MongoUpdate)] = Arbitrary(getDifferentOrdersUpdates)

  "MongoUpdate" should{
    "convert to the same JValue, no matter the order of constructions" in{
      check { updates: (MongoUpdate, MongoUpdate) =>
        updates._1.toJValue == updates._2.toJValue
      }
    }

    "should equal, no matter the order of constructions" in{
      check { updates: (MongoUpdate, MongoUpdate) => updates._1 == updates._2 }
    }

    "should have the same hashCodes, no matter the order of constructions" in{
      check { updates: (MongoUpdate, MongoUpdate) => updates._1.hashCode == updates._1.hashCode }
    }
  }
}
