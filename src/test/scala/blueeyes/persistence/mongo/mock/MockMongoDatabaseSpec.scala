package blueeyes.persistence.mongo.mock

import org.specs.Specification
import blueeyes.persistence.mongo.{MongoPrimitiveString, MongoCollectionReference, MongoImplicits}
import blueeyes.json.{Printer, JsonAST}
import blueeyes.json.JsonAST._
import java.util.concurrent.CountDownLatch
import blueeyes.concurrent.Future

class MockMongoDatabaseSpec extends Specification with MongoImplicits{
  private val mongo     = new MockMongo()

  "create collection" in{
    database.collection("bar") must notBeNull
  }
  "return the same collection for the same name" in{
    database.collection("bar") must be (database.collection("bar"))
  }

  "return all collections" in{
    database.collection("bar")
    database.collections.toList.map(v => (v.name, v.database)) mustEqual(List(("bar", database)))
  }

  "dump empty collections content" in{
    database.collection("bar")
    val b = new StringBuilder()

    database.dump((v: String) => b.append(v))

    b.toString mustEqual(Printer.pretty(JsonAST.render(JObject(List(JField("bar", JArray(Nil)))))))
  }
  "dump not empty collections content" in{
    val jobject  = JObject(JField("foo", JArray(List(JString("1")))) :: JField("baz", JString("foo")) :: Nil)
    val jobject2 = JObject(JField("bar", JArray(List(JString("1")))) :: JField("foo", JString("baz")) :: Nil)
    val future1  = database[JNothing.type](insert(jobject, jobject2).into("bar"))
    future1.value must eventually (beSomething)

    val b = new StringBuilder()
    database.dump((v: String) => b.append(v))

    b.toString mustEqual(Printer.pretty(JsonAST.render(JObject(List(JField("bar", JArray(jobject :: jobject2 :: Nil)))))))
  }

  "adToSet really adds to set for not existsing object" in{
    val future1 = database[JNothing.type](upsert("bar").set("foo" addToSet (MongoPrimitiveString("1"))))
    awaitFuture[JNothing.type](future1)

    val future2 = database(select().from("bar"))
    awaitFuture(future2)

    future2.value.get.iterator.toList mustEqual (JObject(JField("foo", JArray(List(JString("1")))) :: Nil) :: Nil)
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

  private  def awaitFuture[T](future: Future[T]) = {
    val countDown = new CountDownLatch(1)

    future.deliverTo(v => countDown.countDown)

    countDown.await
  }

  private def database = mongo.database("foo").asInstanceOf[MockMongoDatabase]
}