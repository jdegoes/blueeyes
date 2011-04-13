package blueeyes.demo

import blueeyes.concurrent.Future
import blueeyes.BlueEyesServer
import net.lag.configgy.ConfigMap
import blueeyes.json.JsonAST._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.persistence.mongo.MongoImplicits._
import blueeyes.core.http.{HttpRequest, HttpResponse, ChunkReader, BijectionsChunkReader}
import blueeyes.core.http.MimeTypes._
import blueeyes.persistence.mongo.{MongoFilterAll, Mongo, MongoFilter}
import blueeyes.json.{JPathField, JPath}
import blueeyes.persistence.mongo.MockMongo

object BlueEyesDemo extends BlueEyesServer with BlueEyesDemoService {
  lazy val mongo = new MockMongo()
  override def main(args: Array[String]) = super.main(Array("--configFile", "/etc/default/blueeyes.conf"))
}

trait BlueEyesDemoService extends BlueEyesServiceBuilder with HttpRequestCombinators with BijectionsChunkReader{
  def mongo: Mongo

  val contactListService = service("contactlist", "1.0.0") {
    healthMonitor { monitor => context =>
      startup {
        val config = BlueEyesDemoConfig(context.config, mongo)

//        config.database[JNothing.type](ensureUniqueIndex("contacts.name").on(config.collection, "name"))

        config
      } ->
      request { demoConfig: BlueEyesDemoConfig =>
        import demoConfig._

        path("/contacts"){
          produce[ChunkReader, JValue, ChunkReader](application/json) {
            get { request: HttpRequest[ChunkReader] =>
              val contacts = database(select(".name").from(collection)) map {records =>
                JArray(records.flatMap(row => (row \\ "name").value).toList)
              }

              contacts.map(v => HttpResponse[JValue](content=Some(v)))
            }
          } ~
          jvalue[ChunkReader] {
            post {
              refineContentType[JValue, JObject] { request =>
                database[JNothing.type](insert(request.content.get).into(collection))

                HttpResponse[JValue]()
              }
            }
          } ~
          path("/'name") {
            produce[ChunkReader, JValue, ChunkReader](application/json) {
              get { request: HttpRequest[ChunkReader] =>
                val contact = database(selectOne().from(collection).where("name" === request.parameters('name)))

                contact.map(v => HttpResponse[JValue](content=v, status=if (!v.isEmpty) OK else NotFound))
              } ~
              delete { request: HttpRequest[ChunkReader] =>
                database[JNothing.type](remove.from(collection).where("name" === request.parameters('name)))

                HttpResponse[JValue]()
              }
            }
          } ~
          path("/search") {
            jvalue[ChunkReader] {
              post {
                refineContentType[JValue, JObject] { request =>
                  val contacts = searchContacts(request.content, demoConfig)

                  contacts.map(v => HttpResponse[JValue](content=Some(JArray(v))))
                }
              }
            }
          }
        }
      } ->
      shutdown { demoConfig: BlueEyesDemoConfig =>
        // Nothing to do
      }
    }
  }

  private def searchContacts(filterJObject: Option[JObject], config: BlueEyesDemoConfig): Future[List[JString]] = {
    createFilter(filterJObject).map { filter =>
      val nameJObject = config.database(select().from(config.collection).where(filter)).map(_.toList)
      nameJObject.map(_.map(_ \ "name" -->? classOf[JString]).filter(_ != None).map(_.get))
    }.getOrElse(Future.lift(Nil))
  }

  private def createFilter(filterJObject: Option[JObject]) = filterJObject.map {
    _.flatten.collect {
      case f @ JField(_, JString(_)) => f
    }.foldLeft(MongoFilterAll.asInstanceOf[MongoFilter]) { (filter, field) =>
      filter && field.name === field.value.asInstanceOf[JString].value
    }
  }
}

case class BlueEyesDemoConfig(config: ConfigMap, mongo: Mongo){
  val database    = mongo.database(config.getString("mongo.database.contacts").getOrElse("contacts"))
  val collection  = config.getString("mongo.collection.contacts").getOrElse("contacts")
}