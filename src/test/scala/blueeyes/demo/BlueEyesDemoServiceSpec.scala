package blueeyes.demo

import blueeyes.core.service.test.BlueEyesServiceSpecification
import blueeyes.core.http.{HttpStatus, MimeTypes, HttpResponse}
import blueeyes.json.JsonAST.{JValue, JObject, JField, JString, JArray}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.MimeTypes._
import blueeyes.persistence.mongo._
import blueeyes.demo.Serialization._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.data.BijectionsChunkJson

class BlueEyesDemoServiceSpec extends BlueEyesServiceSpecification with BlueEyesDemoService with BijectionsChunkJson with blueeyes.concurrent.test.FutureMatchers {
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
      f.value must eventually(beSome)
    }

    "return contact list" in {
      service.get("/contacts") must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status  mustEqual(HttpStatus(OK))) and
            (content must beSome(JArray(List(contact \\ "name"))))
        }
      }
    }

    "return contact" in {
      service.get("/contacts/Sherlock") must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status  mustEqual(HttpStatus(OK))) and
            (content must beSome(contact.serialize))
        }
      }
    }

    "search contact" in {
      service.contentType[JValue](application/MimeTypes.json).post("/contacts/search")(filter) must whenDelivered {
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status  mustEqual(HttpStatus(OK))) and 
            (content must beSome(JArray(List(contact \\ "name"))))
        }
      }
    }
  }
}
