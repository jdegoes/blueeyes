package blueeyes.persistence.mongo

import RealMongo._
import MongoQueryBuilder._
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JsonAST._
import MongoFilterImplicits._

object MongoDemo{
  private val jObject  = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road")) ::  Nil)) :: Nil)
  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road 1")) ::  Nil)) :: Nil)

  private val collection = "my-collection"
  
  def main(args: Array[String]){
    val realMongo = new RealMongo( "localhost" , 27017 )
    val database  = realMongo.database( "mydb" );

    database[JNothing.type](ensureUniqueIndex("index").on(collection, "address.city", "address.street"))

    println("REMOVED=" + database(remove.from(collection)))

    database[JNothing.type](insert(jObject).into(collection))
 
    println("REMOVED=" + database(remove.from(collection).where("address.city" === "London")))

    database[JNothing.type](insert(jObject).into(collection))

    database[JNothing.type](insert(jObject1).into(collection))
  }
}