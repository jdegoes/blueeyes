package blueeyes.demo

import blueeyes.core.service.test.BlueEyesServiceSpecification
import blueeyes.core.http.{HttpStatus, MimeTypes}
import blueeyes.json.JsonAST.{JValue, JObject, JField, JString, JArray}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.MimeTypes._
import blueeyes.persistence.mongo._
import blueeyes.demo.Serialization._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.data.BijectionsChunkJson

class BlueEyesDemoServiceSpec extends BlueEyesServiceSpecification with BlueEyesDemoService with BijectionsChunkJson{
  private val contact = Contact("Sherlock", Some("sherlock@email.com"), Some("UK"), Some("London"), Some("Baker Street, 221B"))

  override def configuration = """
  services {
    contactlist {
      v1 {
        log {
            level = "info"
            console = true
            use_parents = false
        }
        requestLog {
          enabled = false
        }
        mongo {
          servers    = ["localhost"]
          database   = "mydb"
          collection = "mycollection"

          dropBeforeStart {
            mydb = ["mycollection"]
          }
        }
      }
    }
  }
  """

  lazy val filter: JValue = JObject(List(JField("name", JString("Sherlock"))))

  "Demo Service" should {
    "create contact" in {
      val f = service.header("X-Forwarded-For", "71.196.138.244").header("Content-Type", "application/json").post("/contacts")(contact.serialize)
      f.value must eventually(beSomething)
    }

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
      val f = service.contentType[JValue](application/MimeTypes.json).post("/contacts/search")(filter)
      f.value must eventually(beSomething)

      val response = f.value.get

      response.status  mustEqual(HttpStatus(OK))
      response.content must beSome(JArray(List(contact \\ "name")))
    }
  }
}
