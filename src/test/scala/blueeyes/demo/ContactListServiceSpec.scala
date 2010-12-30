package blueeyes.demo

import blueeyes.core.service.test.BlueEyesServiceSpecification
import blueeyes.persistence.mongo.Mongo
import blueeyes.config.ConfiggyModule
import blueeyes.persistence.mongo.mock.MockMongoModule
import blueeyes.core.http.{HttpStatus, HttpResponse}
import com.google.inject.Guice
import blueeyes.json.JsonAST.{JArray}
import blueeyes.core.http.HttpStatusCodes._

class ContactListServiceSpec extends BlueEyesServiceSpecification[Array[Byte]] with ContactListService{
  path$("/contacts"){
    get${ response: HttpResponse[Array[Byte]] =>
      response.status  mustEqual(HttpStatus(OK))

      val content  = JValueToByteArray.unapply(response.content.get)

      content mustEqual(JArray(Nil))
    }
  } should "return contact list"

  private lazy val injector = Guice.createInjector(new ConfiggyModule(rootConfig), new MockMongoModule)

  lazy val mongo = injector.getInstance(classOf[Mongo])

}