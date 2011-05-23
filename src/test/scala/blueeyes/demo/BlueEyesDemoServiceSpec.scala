package blueeyes.demo

import blueeyes.core.service.test.BlueEyesServiceSpecification
import blueeyes.persistence.mongo.MockMongo
import blueeyes.core.http.{HttpStatus, MimeTypes}
import blueeyes.json.JsonAST.{JValue, JObject, JField, JString, JNothing, JArray}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.MimeTypes._
import blueeyes.persistence.mongo._
import blueeyes.demo.Serialization._
import blueeyes.core.http.MimeTypes._
import blueeyes.concurrent.Future
import blueeyes.core.data.{BijectionsChunkJson, BijectionsIdentity}

class BlueEyesDemoServiceSpec extends BlueEyesServiceSpecification with BlueEyesDemoService with BijectionsChunkJson{
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

      val created = database(select().from(collectionName))

      created.value.map(_.toList.map(_.deserialize[Contact])) must eventually (beSome(List(contact)))
    }
  }

   "Demo Service" should {

     val filter: JValue = JObject(List(JField("name", JString("Sherlock"))))

     val removed = database[JNothing.type](remove.from(collectionName))
     removed.value must eventually (beSomething)
     val inserted = database[JNothing.type](insert(contact.serialize.asInstanceOf[JObject]).into(collectionName))
     inserted.value must eventually (beSomething)

     "return contact list" in {
        val f = service.get("/contacts")
        f.value must eventually(beSomething)

        val response = f.value.get

        response.status  mustEqual(HttpStatus(OK))
        response.content must beSome(JArray(List(contact \\ "name")))
     }
     "return contact" in {
        val f = service.get("/contacts/Sherlock")
        f.value must eventually(beSomething)

        val response = f.value.get

        response.status  mustEqual(HttpStatus(OK))
        response.content must beSome(contact.serialize)
     }
     "search contact" in {
       import BijectionsIdentity._
        val f = service.contentType[JValue](application/MimeTypes.json).post("/contacts/search")(filter)
        f.value must eventually(beSomething)

        val response = f.value.get

        response.status  mustEqual(HttpStatus(OK))
        response.content must beSome(JArray(List(contact \\ "name")))
     }
   }

  lazy val mongo    = new MockMongo()
  lazy val database = mongo.database(databaseName)
}
