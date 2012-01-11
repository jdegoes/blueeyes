package blueeyes.persistence.mongo.mock

import org.specs2.mutable.Specification
import blueeyes.concurrent.test.FutureMatchers
import blueeyes.persistence.mongo.{MongoPrimitiveString, MongoImplicits}
import blueeyes.json.Printer
import blueeyes.json.JsonAST._
import akka.util.Timeout

class MockMongoDatabaseSpec extends Specification with MongoImplicits with FutureMatchers {
  implicit val queryTimeout = Timeout(10000)

  "create collection" in{
    val mongo     = new MockMongo()
    mongoDatabase(mongo).collection("bar") must not beNull
  }
  "return the same collection for the same name" in{
    val mongo     = new MockMongo()
    mongoDatabase(mongo).collection("bar") must be (mongoDatabase(mongo).collection("bar"))
  }

  "return all collections" in{
    val mongo    = new MockMongo()
    val database = mongoDatabase(mongo)
    database.collection("bar")
    database.collections.toList.map(v => (v.name, v.database)) mustEqual(List(("bar", database)))
  }

  "dump empty collections content" in{
    val mongo     = new MockMongo()
    val database = mongoDatabase(mongo)

    database.collection("bar")
    val b = new StringBuilder()

    database.dump((v: String) => b.append(v))

    b.toString mustEqual(Printer.pretty(Printer.render(JObject(List(JField("bar", JArray(Nil)))))))
  }
  "dump not empty collections content" in{
    val mongo     = new MockMongo()
    
    val jobject  = JObject(JField("foo", JArray(List(JString("1")))) :: JField("baz", JString("foo")) :: Nil)
    val jobject2 = JObject(JField("bar", JArray(List(JString("1")))) :: JField("foo", JString("baz")) :: Nil)
    val database: MockDatabase = mongoDatabase(mongo)
    val future1  = database(insert(jobject, jobject2).into("bar"))
    future1.value must eventually (beSome)

    val b = new StringBuilder()
    database.dump((v: String) => b.append(v))

    b.toString mustEqual(Printer.pretty(Printer.render(JObject(List(JField("bar", JArray(jobject :: jobject2 :: Nil)))))))
  }

  "adToSet really adds to set for not existsing object" in{
    val mongo     = new MockMongo()

    val database = mongoDatabase(mongo)
    val future1 = database(upsert("bar").set("foo" addToSet (MongoPrimitiveString("1"))))
    future1.value must eventually (beSome)

    val future2 = database(select().from("bar"))

    future2.map(_.iterator.toList) must whenDelivered(be_==(JObject(JField("foo", JArray(List(JString("1")))) :: Nil) :: Nil))
  }
//  "adToSet really adds to set for existsing object" in{
//    val future1 = database[JNothing.type](insert(JObject(JField("foo", JArray(List(JString("1")))) :: Nil)).into("bar"))
//    awaitFuture[JNothing.type](future1)
//
//    val future2 = database[JNothing.type](upsert("bar").set("foo" addToSet (MongoPrimitiveString("2"))))
//    awaitFuture[JNothing.type](future2)
//
//    val future3 = database(select().from("bar"))
//    awaitFuture(future3)
//
//    future3.value.get.iterator.toList mustEqual (JObject(JField("foo", JArray(List(JString("1"), JString("2")))) :: Nil) :: Nil)
//  }
//  "adToSet does not add duplicate elements" in{
//    val future1 = database[JNothing.type](insert(JObject(JField("foo", JArray(List(JString("1")))) :: Nil)).into("bar"))
//    awaitFuture[JNothing.type](future1)
//
//    val future2 = database[JNothing.type](upsert("bar").set("foo" addToSet (MongoPrimitiveString("1"))))
//    awaitFuture[JNothing.type](future2)
//
//    val future3 = database(select().from("bar"))
//    awaitFuture(future3)
//
//    future3.value.get.iterator.toList mustEqual (JObject(JField("foo", JArray(List(JString("1")))) :: Nil) :: Nil)
//  }
//  "adToSet does not add duplicate elements when update has duplicates" in{
//    val future1 = database[JNothing.type](insert(JObject(JField("foo", JArray(List(JString("1")))) :: Nil)).into("bar"))
//    awaitFuture[JNothing.type](future1)
//
//    val future2 = database[JNothing.type](upsert("bar").set("foo" addToSet (MongoPrimitiveString("1"), MongoPrimitiveString("1"))))
//    awaitFuture[JNothing.type](future2)
//
//    val future3 = database(select().from("bar"))
//    awaitFuture(future3)
//
//    future3.value.get.iterator.toList mustEqual (JObject(JField("foo", JArray(List(JString("1")))) :: Nil) :: Nil)
//  }

  private def mongoDatabase(mongo: MockMongo) = mongo.database("foo").asInstanceOf[MockDatabase]
}
