package blueeyes.persistence.mongo

import com.mongodb._
import org.specs.Specification

import java.util.regex.Pattern
import java.util.regex.Pattern._

import scala.collection.JavaConversions._
import java.util.{HashMap, Map}
import blueeyes.json.JsonAST._
import json.BijectionsMongoJson._

//class MongoValuesSpec extends Specification{
////  "Test mongo regex" in{
////    val collection = createCollection
////
////    val john  = Pattern.compile("joh?n", CASE_INSENSITIVE)
////    val query = new BasicDBObject("name", john)
////
////    val result = collection.find(query).toArray
////
////    result foreach println
////  }
//  "Test mongo date" in{
//    val collection = createCollection
//
////    val jobject = JObject(List(JField("family", JObject(List(JField("name", JString("foo")))))))
//
//    val john  = Pattern.compile("joh?n", CASE_INSENSITIVE)
//    val john1  = Pattern.compile("joh?n")
//    println("PATTERN=" + john1.pattern())
//    println("FLAGS=" + john1.flags())
//    val now = new java.util.Date()
//    val map = new HashMap[String, Any]()
//    map.put("ts", now)
//    map.put("binary", "bin".getBytes)
//    map.put("string", "Value")
//    map.put("john", john)
//    val time = new BasicDBObject(map)
//
//    val root = new BasicDBObject("data", time)
//
//    collection.save(root)
////    collection.save(jobject)
//    val result = collection.find(new BasicDBObject()).toArray
//
//    val robject= result.head
//    val value = robject.get("data").asInstanceOf[BasicDBObject].get("binary")
//    value match {
//      case x: Array[Byte] => println("BYTES")
//      case _ => println("another")
//    }
//
//
//    val converted = result.map(v => MongoValueBijection.converter.apply(v))
//    converted.foreach(v => collection.save(MongoValueBijection.converter.unapply(v)))
//
//    val result1 = collection.find(new BasicDBObject()).toArray
//    val converted1 = result1.map(v => MongoValueBijection.converter.apply(v))
//
//    result1 foreach println
//    println("-------------------")
//    converted1 foreach println
//
//    collection.remove(new BasicDBObject())
//  }
//
//  private def createCollection = {
//    val options = new MongoOptions()
//    options.connectionsPerHost = 1000
//    options.threadsAllowedToBlockForConnectionMultiplier = 1000
//    val server = new ServerAddress("localhost", ServerAddress.defaultPort())
//
//    val mongo      = new com.mongodb.Mongo(server, options)
//    val database   = mongo.getDB("test")
//    database.getCollection("collection")
//  }
//}