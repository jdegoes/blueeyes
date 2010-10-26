package blueeyes.persistence.mongo

import RealMongo._
import MongoQueryBuilder._
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JsonAST._
import MongoFilterImplicits._

object MongoTest{
  private val jObject = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road")) ::  Nil)) :: Nil)

  private val collection = "my-collection"
  def main(args: Array[String]){
    val realMongo = new RealMongo( "localhost" , 27017 )
    val db        = realMongo.database( "mydb" );

    val indexQuery: MongoQuery[JNothing.type] = ensureUniqueIndex("index").on(collection, "address.city", "address.street")
    db[JNothing.type](indexQuery)

    val removeQuery: MongoQuery[JNothing.type] = remove.from(collection)
    db[JNothing.type](removeQuery)

    val insertQuery: MongoQuery[JNothing.type] = insert(jObject).into(collection)
    db[JNothing.type](insertQuery)

    val removeQuery1: MongoQuery[JNothing.type] = remove.from(collection).where("address.city" === "London")
    db[JNothing.type](removeQuery1)

    val insertQuery1: MongoQuery[JNothing.type] = insert(jObject).into(collection)
    db[JNothing.type](insertQuery1)
  }
}