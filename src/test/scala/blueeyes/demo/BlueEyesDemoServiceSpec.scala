package blueeyes.demo

import blueeyes.core.service.test.BlueEyesServiceSpecification
import blueeyes.persistence.mongo.Mongo
import blueeyes.config.ConfiggyModule
import blueeyes.persistence.mongo.mock.MockMongoModule
import blueeyes.core.http.{HttpStatus, HttpResponse}
import com.google.inject.Guice
import blueeyes.json.JsonAST.{JValue, JArray}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.MimeTypes._

class BlueEyesDemoServiceSpec extends BlueEyesServiceSpecification[Array[Byte]] with BlueEyesDemoService{
  private val contact = Contact("Sherlock Holmes", Some("sherlock@email.com"), Some("UK"), Some("London"), Some("Baker Street, 221B"))

  "BlueEyesDemoService" in {
    path$("/contacts"){
      contentType$[JValue, Array[Byte], JValue](application/json){
        get${ response: HttpResponse[JValue] =>
          response.status  mustEqual(HttpStatus(OK))
          response.content must beSome(JArray(Nil))
        }
      }
    } should "return contact list"
  }


  private lazy val injector = Guice.createInjector(new ConfiggyModule(rootConfig), new MockMongoModule)

  lazy val mongo = injector.getInstance(classOf[Mongo])

}