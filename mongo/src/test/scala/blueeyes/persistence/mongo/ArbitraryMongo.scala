package blueeyes.persistence.mongo

import org.scalacheck.Gen

import MongoFilterOperators._
import dsl._

import blueeyes.json.{ArbitraryJValue, JPath}

trait ArbitraryMongo extends ArbitraryJValue{
  def getMongoFieldFilter[Gen[MongoFieldFilter]] = for{
    path     <- Gen.listOfN(Gen.choose(5, 10).sample.get, Gen.alphaChar).map(chars => JPath(new String(chars.toArray)))
    operator <- Gen.oneOf($eq, $ne, $gt, $gte, $lt, $lte, $in, $all, $size, $exists, $type)
    value    <- genSimple
  } yield {MongoFieldFilter(path, operator, value)}

  def getListMongoFieldFilter = Gen.containerOfN[List, MongoFieldFilter](Gen.choose(2, 8).sample.get, getMongoFieldFilter)

  def getMongoUpdate[Gen[MongoUpdate]] = for{
    path    <- Gen.listOfN(Gen.choose(5, 10).sample.get, Gen.alphaChar).map(chars => JPath(new String(chars.toArray)))
    value   <- genSimple
    jobject <- genObject
    update  <- Gen.oneOf[MongoUpdate](MongoUpdateObject(jobject), path.inc(value), path.set(value), path.unset, path.popLast, path.popFirst, path.push(value), path.pull("" === "foo"), path.pushAll(jvalueToMongoPrimitive(value)), path.pullAll(jvalueToMongoPrimitive(value)), path.addToSet(jvalueToMongoPrimitive(value)))
  } yield {update}

  def getListMongoUpdate = Gen.containerOfN[List, MongoUpdate](Gen.choose(2, 8).sample.get, getMongoUpdate)

}
