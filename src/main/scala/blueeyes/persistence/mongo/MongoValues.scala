package blueeyes.persistence.mongo

import com.mongodb.DBObject


//object MongoValues {
//  implicit def mongoObject2MongoValue(dbObject: DBObject): MongoValue = {
//    val allKeys  = asScalaSet(dbObject.keySet)
//    val pureKeys = allKeys.filter(_ != "_id")
//
//    def toJField(key: String): JField = {
//      JField(key, anyRef2JValue(dbObject.get(key)))
//    }
//
//    JObject(pureKeys.map(toJField(_)).toList)
//  }
//
//}