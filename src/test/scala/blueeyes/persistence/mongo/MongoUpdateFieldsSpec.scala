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

class MongoUpdateFieldsSpec extends Specification with ScalaCheck with MongoImplicits with ArbitraryJValue{

  def getMongoUpdateField[Gen[MongoUpdateField]] = for{
    path     <- Gen.listOfN(Gen.choose(5, 10).sample.get, Gen.alphaChar).map(chars => JPath(new String(chars.toArray)))
    value    <- genSimple
    update <- Gen.oneOf[MongoUpdateField](path.inc(value), path.set(value), path.unset, path.popLast, path.popFirst, path.push(value), path.pull("" === "foo"), path.pushAll(jvalueToMongoPrimitive(value)), path.pullAll(jvalueToMongoPrimitive(value)), path.addToSet(jvalueToMongoPrimitive(value)))
  } yield {update}

  def getListMongoUpdateField = Gen.containerOfN[List, MongoUpdateField](Gen.choose(2, 8).sample.get, getMongoUpdateField)

  def getDifferentOrdersUpdates: Gen[(MongoUpdateFields, MongoUpdateFields)] = getListMongoUpdateField.map{updates =>
    def andUpdate(values: List[MongoUpdateField]) = {values.tail.foldLeft(MongoUpdateFields(Set(values.head))){(andUpdate, update) => (andUpdate & update).asInstanceOf[MongoUpdateFields]}    }
    (andUpdate(updates), andUpdate(updates.reverse))
  }

  implicit def arbDifferentOrdersAnds: Arbitrary[(MongoUpdateFields, MongoUpdateFields)] = Arbitrary(getDifferentOrdersUpdates)

  "MongoUpdateFields" should{
    "convert to the same JValue, no matter the order of constructions" in{
      forAll { updates: (MongoUpdateFields, MongoUpdateFields) =>
        updates._1.toJValue == updates._2.toJValue
      } must pass
    }

    "should equal, no matter the order of constructions" in{
      forAll { updates: (MongoUpdateFields, MongoUpdateFields) => updates._1 == updates._2 } must pass
    }

    "should have the same hashCodes, no matter the order of constructions" in{
      forAll { updates: (MongoUpdateFields, MongoUpdateFields) => updates._1.hashCode == updates._1.hashCode } must pass
    }

    "build valid json with several MongoUpdateField" in{
      (("x" inc (1)) & ("y" set (1))).toJValue mustEqual (JObject(JField("$inc", JObject(JField("x", JInt(1)) :: Nil)) :: JField("$set", JObject(JField("y", JInt(1)) :: Nil)) :: Nil))
    }

    "build valid json with 3 MongoUpdateFieldValues" in {
      (("x" inc (1)) & ("y" set (1)) & ("z" inc (1))).toJValue mustEqual (JObject(JField("$inc", JObject(JField("x", JInt(1)) :: JField("z", JInt(1)):: Nil)) :: JField("$set", JObject(JField("y", JInt(1)) :: Nil)) :: Nil))
    }

    "build valid json with 2 & 2 MongoUpdateFieldsValuess" in {
      (("x" inc (1)) & ("y" set (1)) & (("z" inc (1))) & ("b" inc(1))).toJValue mustEqual (JObject(JField("$inc", JObject(JField("x", JInt(1)) :: JField("z", JInt(1)) :: JField("b", JInt(1)) :: Nil)) :: JField("$set", JObject(JField("y", JInt(1)) :: Nil)) :: Nil))
    }
  }
}

//  def inc(value: MongoPrimitive) : MongoUpdateField = IncF(jpath, value)
//  def set(value: MongoPrimitive) : MongoUpdateField = SetF(jpath, value)
//  def unset                             : MongoUpdateField = UnsetF(jpath)
//  def popLast                           : MongoUpdateField = PopLastF(jpath)
//  def popFirst                          : MongoUpdateField = PopFirstF(jpath)
//  def push [T](value: MongoPrimitive): MongoUpdateField = PushF(jpath, value)
//  def pull(filter: MongoFilter)         : MongoUpdateField = PullF(jpath, filter)
//  def pushAll [T <: MongoPrimitive](items: T*) : MongoUpdateField = PushAllF(jpath, List(items: _*))
//  def pullAll [T <: MongoPrimitive](items: T*) : MongoUpdateField = PullAllF(jpath, List(items: _*))
//
//  def addToSet [T <: MongoPrimitive](items: T*): MongoUpdateField = {
