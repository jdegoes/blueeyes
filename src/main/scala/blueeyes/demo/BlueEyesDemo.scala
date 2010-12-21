package blueeyes.demo

import blueeyes.config.ConfiggyModule
import blueeyes.persistence.mongo.mock.MockMongoModule
import blueeyes.persistence.mongo.Mongo
import com.google.inject.Guice
import blueeyes.BlueEyesServer
import net.lag.configgy.ConfigMap
import blueeyes.json.JsonAST._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.persistence.mongo.MongoQueryBuilder._
import blueeyes.persistence.mongo.MongoImplicits._
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.core.http.MimeTypes._

object BlueEyesDemo extends BlueEyesServer with ContactListService{

  private lazy val injector = Guice.createInjector(new ConfiggyModule(rootConfig), new MockMongoModule)

  lazy val mongo = injector.getInstance(classOf[Mongo])

  override def main(args: Array[String]) = super.main(Array("--configFile", "/etc/default/blueeyes.conf"))
}

trait ContactListService extends BlueEyesServiceBuilder with HttpRequestCombinators {

  def mongo: Mongo

  val contactListService = service("contact.list", "1.0.0") {
    context => {
      startup {
        val config = ContactListConfig(context.config, mongo)

        config.database[JNothing.type](ensureUniqueIndex("contact.list.name").on(config.collection, "name"))

        config
      } -> {
        request { config: ContactListConfig =>
          import config._
          path("/contacts"){
            produce(application/json) {
              get {
                request: HttpRequest[Array[Byte]] => {
                  val names = JArray(database(select(".name").from(collection)).flatMap(
                  row => (row \\ "name").value).toList)
                  HttpResponse[JValue](content=Some(names))
                }
              }
            } ~
            path("/'name") {
              produce(application/json) {
                get {
                  request: HttpRequest[Array[Byte]] => {
                    val contact = database(selectOne().from(collection).where("name" === request.parameters('name)))
                    val status = if(!contact.isEmpty) OK else NotFound
                    HttpResponse[JValue](content=contact, status=status)
                  }
                } ~
                delete {
                  request: HttpRequest[Array[Byte]] => {
                    database[JNothing.type](remove.from(collection).where("name" === request.parameters('name)))
                    HttpResponse[JValue]()
                  }
                }
              }
            } ~
            jvalue {
              post {
                refineContentType[JValue, JObject] {
                  requireContent((j: JObject) => !((j \ "name" -->? classOf[JString]).isEmpty)) {
                    request => {
                      val name = (request.content.get \ "name" --> classOf[JString]).value
                      database[JNothing.type](upsert(collection).set(request.content.get).where("name" === name))
                      HttpResponse[JValue]()
                    }
                  }
                }
              }
            }
          }
        }
      }->
        shutdown { config: ContactListConfig =>
      }
    }
  }
}

case class ContactListConfig(config: ConfigMap, mongo: Mongo){
  val database    = mongo.database(config.getString("mongo.database.contact.list").getOrElse("contact.list"))
  val collection  = config.getString("mongo.collection.contact.list").getOrElse("contact.list")
}