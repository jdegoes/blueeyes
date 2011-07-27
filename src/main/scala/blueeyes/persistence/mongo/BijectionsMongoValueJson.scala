package blueeyes.persistence.mongo

import blueeyes.core.data.Bijection
import json.BijectionsMongoJson
import scalaz.Validation
import blueeyes.json.JsonAST.JValue

//
//trait BijectionsMongoValueJson extends BijectionsMongoValue with BijectionsMongoJson{
//  implicit val MongoValueToJson = new Bijection[MongoValue, Validation[Throwable, JValue]]{
//    def apply(mongoValue: MongoValue) = try {
//      MongoToJson(MongoValueToMongo(mongoValue))
//    }
//    catch {
//      case e: Throwable =>
//    }
//
//    def unapply(s: Validation[String, JValue]) = null
//  }
//
//  implicit val JsonToMongoValue = new Bijection[JValue, MongoValue]{
//    def apply(t: JValue) = null
//
//    def unapply(s: MongoValue) = null
//  }
//}
//
//
//object BijectionsMongoValueJson extends BijectionsMongoValueJson