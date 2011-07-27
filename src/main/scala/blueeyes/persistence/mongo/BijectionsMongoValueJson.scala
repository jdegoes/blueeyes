package blueeyes.persistence.mongo

import blueeyes.core.data.Bijection
import scalaz.Validation
import blueeyes.json.JsonAST.JValue


//trait BijectionsMongoValueJson {
//  implicit val MongoValueToJson = new Bijection[MongoValue, Validation[String, JValue]]{
//    def apply(mongoValue: MongoValue) = null
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