package blueeyes.persistence.mongo

import org.scalacheck.Gen

import MongoFilterOperators._
import blueeyes.json.{ArbitraryJValue, JPath}

trait ArbitraryMongoFilter extends ArbitraryJValue{
  def getMongoFieldFilter[Gen[MongoFieldFilter]] = for{
    path     <- Gen.listOfN(Gen.choose(5, 10).sample.get, Gen.alphaChar).map(chars => JPath(new String(chars.toArray)))
    operator <- Gen.oneOf($eq, $ne, $gt, $gte, $lt, $lte, $in, $all, $size, $exists, $type)
    value    <- genSimple
  } yield {MongoFieldFilter(path, operator, value)}

  def getListMongoFieldFilter = Gen.containerOfN[List, MongoFieldFilter](Gen.choose(2, 8).sample.get, getMongoFieldFilter)

}