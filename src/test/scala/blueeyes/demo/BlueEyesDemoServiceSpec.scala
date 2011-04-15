package blueeyes.demo

import java.util.concurrent.CountDownLatch
import blueeyes.core.service.test.BlueEyesServiceSpecification
import blueeyes.persistence.mongo.{Mongo, MockMongo}
import blueeyes.core.http.{HttpStatus, HttpResponse, MimeTypes}
import blueeyes.json.JsonAST.{JValue, JObject, JField, JString, JNothing, JArray}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.MimeTypes._
import blueeyes.persistence.mongo._
import blueeyes.demo.Serialization._
import blueeyes.core.http.MimeTypes._
import blueeyes.concurrent.Future
import blueeyes.core.service.HttpClient
import blueeyes.core.data.{Chunk, BijectionsChunkReaderJson, Bijection}

class BlueEyesDemoServiceSpec extends BlueEyesServiceSpecification with BlueEyesDemoService with BijectionsChunkReaderJson{
  private val contact = Contact("Sherlock", Some("sherlock@email.com"), Some("UK"), Some("London"), Some("Baker Street, 221B"))

  private val databaseName   = "mydb"
  private val collectionName = "nycollection"

  override def configuration = """
  services {
    contactlist {
      v1 {
        mongo {
          database{
            contacts = "%s"
          }
          collection{
            contacts = "%s"
          }
        }    
      }
    }
  }
  """.format(databaseName, collectionName)

  "Demo Service" should {
    "create contact" in {
      val f = service.post("/contacts")(contact.serialize)
      f.value must eventually(beSomething)

      val response = f.value.get

      val countDown = new CountDownLatch(1)

      val created = database(select().from(collectionName))
      created.deliverTo{v =>
        countDown.countDown()
      }
      countDown.await()

      created.value.get.toList.map(_.deserialize[Contact]) mustEqual(List(contact))
    }
  }

   "Demo Service" should {
     def awaitResult[T](future: Future[T]) = {
       val countDown = new CountDownLatch(1)
       future deliverTo {v => countDown.countDown()}
       countDown.await()
     }

     val filter: JValue = JObject(List(JField("name", JString("Sherlock"))))

     val removed  = awaitResult[JNothing.type](database[JNothing.type](remove.from(collectionName)))
     val inserted = awaitResult[JNothing.type](database[JNothing.type](insert(contact.serialize.asInstanceOf[JObject]).into(collectionName)))

     def client = service.contentType[JValue](application/MimeTypes.json)
     "return contact list" in {
        val f = client.get("/contacts")
        f.value must eventually(beSomething)

        val response = f.value.get

        response.status  mustEqual(HttpStatus(OK))
        response.content must beSome(JArray(List(contact \\ "name")))
     }
     "return contact list" in {
        val f = client.get("/contacts/Sherlock")
        f.value must eventually(beSomething)

        val response = f.value.get

        response.status  mustEqual(HttpStatus(OK))
        response.content must beSome(contact.serialize)
     }
     "search contact" in {
        val f = client.post("/contacts/search")(filter)
        f.value must eventually(beSomething)

        val response = f.value.get

        response.status  mustEqual(HttpStatus(OK))
        response.content must beSome(JArray(List(contact \\ "name")))
     }
   }

  lazy val mongo    = new MockMongo()
  lazy val database = mongo.database(databaseName)
}
