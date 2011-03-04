package blueeyes.demo

import java.util.concurrent.CountDownLatch
import blueeyes.core.service.test.BlueEyesServiceSpecification
import blueeyes.persistence.mongo.Mongo
import blueeyes.config.ConfiggyModule
import blueeyes.persistence.mongo.mock.MockMongoModule
import blueeyes.core.http.{HttpStatus, HttpResponse, MimeTypes}
import com.google.inject.Guice
import blueeyes.json.JsonAST.{JValue, JObject, JField, JString, JNothing, JArray}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.MimeTypes._
import blueeyes.persistence.mongo._
import blueeyes.demo.Serialization._
import blueeyes.util.Future

//import Extractors._

class BlueEyesDemoServiceSpec extends BlueEyesServiceSpecification[Array[Byte]] with BlueEyesDemoService{
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

  "BlueEyesDemoService" in {
    path$("/contacts") {
      contentType$[JValue, Array[Byte], Unit](application/MimeTypes.json){
        post$(contact.serialize){ response: HttpResponse[JValue] =>

          response.status  mustEqual(HttpStatus(OK))
          response.content must beNone

          val countDown = new CountDownLatch(1)

          val created = database(select().from(collectionName))
          created.deliverTo{v =>
            countDown.countDown()
          }
          countDown.await()

          created.value.get.toList.map(_.deserialize[Contact]) mustEqual(List(contact))
        }
      }
    } should "create contact"
  }

  "BlueEyesDemoService" in {
    def awaitResult[T](future: Future[T]) = {
      val countDown = new CountDownLatch(1)
      future deliverTo {v => countDown.countDown()}
      countDown.await()
    }

    val filter: JValue = JObject(List(JField("name", JString("Sherlock"))))
    
    val removed  = awaitResult[JNothing.type](database[JNothing.type](remove.from(collectionName)))
    val inserted = awaitResult[JNothing.type](database[JNothing.type](insert(contact.serialize.asInstanceOf[JObject]).into(collectionName)))

    path$("/contacts"){
      contentType$[JValue, Array[Byte], Unit](application/MimeTypes.json){
        get$ { response: HttpResponse[JValue] =>
          response.status  mustEqual(HttpStatus(OK))
          response.content must beSome(JArray(List(contact \\ "name")))
        }
      }
    } should "return contact list"

    path$("/contacts/Sherlock"){
      contentType$[JValue, Array[Byte], Unit](application/MimeTypes.json){
        get$ { response: HttpResponse[JValue] =>
          response.status  mustEqual(HttpStatus(OK))
          response.content must beSome(contact.serialize)
        }
      }
    } should "return contact by name"

    path$("/contacts/search"){
      contentType$[JValue, Array[Byte], Unit](application/MimeTypes.json){
        post$(filter) { response: HttpResponse[JValue] =>
          response.status  mustEqual(HttpStatus(OK))
          response.content must beSome(JArray(List(contact \\ "name")))
        }
      }
    } should "search contact"
  }

  private lazy val injector = Guice.createInjector(new ConfiggyModule(rootConfig), new MockMongoModule)

  lazy val mongo    = injector.getInstance(classOf[Mongo])
  lazy val database = mongo.database(databaseName)
}
