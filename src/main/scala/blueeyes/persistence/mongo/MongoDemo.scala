package blueeyes.persistence.mongo

import MongoQueryBuilder._
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JsonAST._
import MongoFilterImplicits._
import blueeyes.config.{ConfiggyModule, FilesystemConfiggyModule}
import com.google.inject.Guice
import blueeyes.json.{JPath, Printer}

object MongoDemo{
  private val jObject  = JObject(JField("address", JObject( JField("city", JString("A")) :: JField("street", JString("1")) :: JField("code", JInt(1)) :: Nil)) :: Nil)
  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("2")) :: JField("code", JInt(1)) :: Nil)) :: Nil)
  private val jObject2 = JObject(JField("address", JObject( JField("city", JString("C")) :: JField("street", JString("3")) :: JField("code", JInt(1)) :: Nil)) :: Nil)
  private val jObject3 = JObject(JField("address", JObject( JField("city", JString("E")) :: JField("street", JString("4")) :: JField("code", JInt(1)) :: Nil)) :: Nil)
  private val jObject6 = JObject(JField("address", JString("ll")) :: Nil)

  private val collection = "my-collection"

  lazy val injector = Guice.createInjector(new FilesystemConfiggyModule(ConfiggyModule.FileLoc), new RealMongoModule)

  val realMongo = injector.getInstance(classOf[Mongo])

  val database  = realMongo.database( "mydb" );
  
  def main(args: Array[String]){

    println(JString("foo").get(JPath("")))

//    database(remove.from(collection))

//    database[JNothing.type](ensureUniqueIndex("index").on(collection, "address.city", "address.street"))

//    demoSelectOne

//    demoSelect
    
    demoUpdate

//    demoRemove
  }

  private def demoSelect{
    println("------------demoSelect------------------")
    insertObjects

//    printObjects(database(select().from(collection).sortBy("address.street" <<)))
//    printObjects(database(select().from(collection).sortBy("address.street" >>)))
//    printObjects(database(select().from(collection).sortBy("address.city" >>).skip(1).limit(1)))
//    printObjects(database(select().from(collection).where("address.city" === "B").sortBy("address.city" >>)))
//    printObjects(database(select().from(collection).where("address.city" === "Z").sortBy("address.city" >>)))
//    printObjects(database(select("address.city").from(collection).sortBy("address.city" >>)))
    println(database(count.from(collection)))
//    printObjects(database(select("address.city").from(collection).sortBy("address.city" <<)))

    database(remove.from(collection))
    println("------------demoSelect------------------")
  }
  private def demoUpdate{
    import MongoUpdateBuilder._
    println("------------demoUpdate------------------")
    insertObjects

//    println(database(updateMany(collection).set("address" popFirst)))
//    printObjects(database(select().from(collection)))
    println(database(update(collection).set(("address.city" unset) & ("address.street" set ("Another Street"))).where("address.city" === "C")))
    printObjects(database(select().from(collection)))
//    println(database(update(collection).set(jObject3).where("address.city" === "A")))
//    printObjects(database(select().from(collection)))
//    println(database(updateMany(collection).set("address.street" set ("New Street"))))
//    printObjects(database(select().from(collection)))

    database(remove.from(collection))
    println("------------demoUpdate------------------")
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

  private def printObjects(objects: Stream[JObject]){
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
    database[JNothing.type](insert(jObject, jObject1, jObject2, jObject3).into(collection))
  }
}