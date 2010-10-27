package blueeyes.persistence.mongo

import RealMongo._
import MongoQueryBuilder._
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JsonAST._
import MongoFilterImplicits._
import blueeyes.json.Printer

object MongoDemo{
  private val jObject  = JObject(JField("address", JObject( JField("city", JString("A")) :: JField("street", JString("1")) ::  Nil)) :: Nil)
  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("2")) ::  Nil)) :: Nil)
  private val jObject2 = JObject(JField("address", JObject( JField("city", JString("C")) :: JField("street", JString("3")) ::  Nil)) :: Nil)

  private val collection = "my-collection"

  val realMongo = new RealMongo( "localhost" , 27017 )
  val database  = realMongo.database( "mydb" );  
  
  def main(args: Array[String]){

    database(remove.from(collection))

    database[JNothing.type](ensureUniqueIndex("index").on(collection, "address.city", "address.street"))

    demoSelectOne

    demoSelect    

    demoRemove
  }

  private def demoSelect{
    println("------------demoSelect------------------")
    insertObjects

    printObjects(database(select().from(collection).sortBy("address.city" <<)))
    printObjects(database(select().from(collection).sortBy("address.city" >>)))
    printObjects(database(select().from(collection).sortBy("address.city" >>).skip(1).limit(1)))
    printObjects(database(select().from(collection).where("address.city" === "B").sortBy("address.city" >>)))
    printObjects(database(select().from(collection).where("address.city" === "Z").sortBy("address.city" >>)))
    printObjects(database(select("address.city").from(collection).sortBy("address.city" >>)))
    printObjects(database(select("address.city").from(collection).sortBy("address.city" <<)))

    database(remove.from(collection))
    println("------------demoSelect------------------")
  }
  private def demoSelectOne{
    println("------------demoSelectOne------------------")
    insertObjects

    printObject(database(selectOne().from(collection).sortBy("address.city" <<)))
    printObject(database(selectOne().from(collection).sortBy("address.city" >>)))
    printObject(database(selectOne().from(collection).where("address.city" === "B").sortBy("address.city" >>)))
    printObject(database(selectOne("address.city").from(collection).sortBy("address.city" >>)))
    printObject(database(selectOne("address.city").from(collection).sortBy("address.city" <<)))

    database(remove.from(collection))
    println("------------demoSelectOne------------------")
  }

  private def printObjects(objects: List[JObject]){
    println("------------------------------------------------")
    println(objects.map(v => Printer.pretty(render(v))).mkString("\n"))
    println("------------------------------------------------")
  }

  private def printObject(objects: Option[JObject]){
    println("------------------------------------------------")
    println(objects.map(v => Printer.pretty(render(v))).mkString("\n"))
    println("------------------------------------------------")
  }

  private def demoRemove{
    println("------------demoRemove------------------")

    insertObjects

    println("REMOVED=" + database(remove.from(collection).where("address.city" === "A")))

    println("REMOVED=" + database(remove.from(collection)))

    println("------------demoRemove------------------")
  }

  private def insertObjects{
    database[JNothing.type](insert(jObject2, jObject, jObject1).into(collection))
  }
}