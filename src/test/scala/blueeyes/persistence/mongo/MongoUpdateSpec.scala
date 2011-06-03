package blueeyes.persistence.mongo

import org.specs.{ScalaCheck, Specification}
import org.scalacheck.Gen
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop._

import MongoUpdateOperators._
import MongoFilterOperators._
import blueeyes.json.JsonAST._
import blueeyes.json.{ArbitraryJValue, JPath}

class MongoUpdateSpec extends Specification with ScalaCheck with MongoImplicits with ArbitraryJValue{

  def getMongoUpdateField[Gen[MongoUpdate]] = for{
    path    <- Gen.listOfN(Gen.choose(5, 10).sample.get, Gen.alphaChar).map(chars => JPath(new String(chars.toArray)))
    value   <- genSimple
    jobject <- genObject
    update  <- Gen.oneOf[MongoUpdate](MongoUpdateObject(jobject), path.inc(value), path.set(value), path.unset, path.popLast, path.popFirst, path.push(value), path.pull("" === "foo"), path.pushAll(jvalueToMongoPrimitive(value)), path.pullAll(jvalueToMongoPrimitive(value)), path.addToSet(jvalueToMongoPrimitive(value)))
  } yield {update}

  def getListMongoUpdate = Gen.containerOfN[List, MongoUpdate](Gen.choose(2, 8).sample.get, getMongoUpdateField)

  def getDifferentOrdersUpdates: Gen[(MongoUpdate, MongoUpdate)] = getListMongoUpdate.map{updates =>
    def andUpdate(values: List[MongoUpdate]) = {values.foldLeft(MongoUpdateNothing.asInstanceOf[MongoUpdate]){(andUpdate, update) => (andUpdate & update)}    }
    (andUpdate(updates), andUpdate(updates.reverse))
  }

  implicit def arbDifferentOrdersAnds: Arbitrary[(MongoUpdate, MongoUpdate)] = Arbitrary(getDifferentOrdersUpdates)

  "MongoUpdate" should{
    "convert to the same JValue, no matter the order of constructions" in{
      forAll { updates: (MongoUpdate, MongoUpdate) =>
        updates._1.toJValue == updates._2.toJValue
      } must pass
    }

    "should equal, no matter the order of constructions" in{
      forAll { updates: (MongoUpdate, MongoUpdate) => updates._1 == updates._2 } must pass
    }

    "should have the same hashCodes, no matter the order of constructions" in{
      forAll { updates: (MongoUpdate, MongoUpdate) => updates._1.hashCode == updates._1.hashCode } must pass
    }
  }
}