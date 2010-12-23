package blueeyes.demo

import blueeyes.config.ConfiggyModule
import blueeyes.persistence.mongo.mock.MockMongoModule
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
import blueeyes.persistence.mongo.{MongoFilterAll, Mongo, MongoFilter}

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
        request { conatctsConfig: ContactListConfig =>
          import conatctsConfig._
          path("/contacts"){
            produce(application/json) {
              get {
                request: HttpRequest[Array[Byte]] => {
                  val contacts = JArray(database(select(".name").from(collection)).flatMap(
                  row => (row \\ "name").value).toList)
                  HttpResponse[JValue](content=Some(contacts))
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
            path("/search") {
              produce(application/json) {
                jvalue {
                  post {
                    refineContentType[JValue, JObject] { request =>
                      val contacts = JArray(searchContacts(request.content, conatctsConfig))
                      HttpResponse[JValue](content=Some(contacts))
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

  private def searchContacts(filterJObject: Option[JObject], config: ContactListConfig): List[JString] = {
    createFilter(filterJObject).map { filter =>
      for {
        nameJObject <- config.database(select().from(config.collection).where(filter)).toList
        name <- nameJObject \ "name" -->? classOf[JString]
      } yield name
    }.getOrElse(Nil)
  }

  private def createFilter(filterJObject: Option[JObject]) = filterJObject.map {
    _.flatten.collect {
      case f @ JField(_, JString(_)) => f
    }.foldLeft(MongoFilterAll.asInstanceOf[MongoFilter]) { (filter, field) =>
      filter && field.name === field.value.asInstanceOf[JString].value
    }
  }
}

case class ContactListConfig(config: ConfigMap, mongo: Mongo){
  val database    = mongo.database(config.getString("mongo.database.contact.list").getOrElse("contact.list"))
  val collection  = config.getString("mongo.collection.contact.list").getOrElse("contact.list")
}