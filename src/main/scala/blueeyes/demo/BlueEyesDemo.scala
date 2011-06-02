package blueeyes.demo

import net.lag.configgy.ConfigMap
import blueeyes.BlueEyesServer
import blueeyes.BlueEyesServiceBuilder
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import blueeyes.core.data.{ByteChunk, BijectionsChunkJson}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.http.MimeTypes._
import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo.MongoImplicits._
import blueeyes.persistence.mongo.{ConfigurableMongo, MongoFilterAll, Mongo, MongoFilter}
import blueeyes.core.service.ServerHealthMonitorService
import blueeyes.core.http.{HttpStatus, HttpRequest, HttpResponse}

object BlueEyesDemo extends BlueEyesServer with BlueEyesDemoService with ServerHealthMonitorService{
  override def main(args: Array[String]) = super.main(Array("--configFile", "/etc/default/blueeyes.conf"))
}

trait BlueEyesDemoService extends BlueEyesServiceBuilder with HttpRequestCombinators with BijectionsChunkJson with ConfigurableMongo{
  val contactListService = service("contactlist", "1.0.0") {
    healthMonitor { monitor => context =>
      startup {
        val mongoConfig = context.config.configMap("mongo")
        BlueEyesDemoConfig(mongoConfig, mongo(mongoConfig)).future
      } ->
      request { demoConfig: BlueEyesDemoConfig =>
        import demoConfig._

        path("/contacts"){
          produce(application/json) {
            get { request: HttpRequest[ByteChunk] =>
              database(select(".name").from(collection)) map { records =>
                HttpResponse[JValue](
                  content = Some(JArray(records.map(_ \\ "name").toList))
                )
              }
            }
          } ~
          aggregate(None){
            post { request =>
              val contact = request.content.map(ChunkToJValue(_))
              contact.map{value =>
                database(insert(value.asInstanceOf[JObject]).into(collection)) map {_ => HttpResponse[ByteChunk]() }
              }.getOrElse(Future.sync(HttpResponse[ByteChunk](status = HttpStatus(BadRequest))))
            }
          } ~
          path("/'name") {
            produce(application/json) {
              get { request: HttpRequest[ByteChunk] =>
                database(selectOne().from(collection).where("name" === request.parameters('name))) map {
                  v => HttpResponse[JValue](content=v, status=if (!v.isEmpty) OK else NotFound)
                }
              } ~
              delete { request: HttpRequest[ByteChunk] =>
                database(remove.from(collection).where("name" === request.parameters('name))) map {
                  _ => HttpResponse[JNothing.type]()
                }
              }
            }
          } ~
          path("/search") {
            jvalue{
              post {
                refineContentType[JValue, JObject] { request =>
                  searchContacts(request.content, demoConfig) map {
                    v => HttpResponse[JValue](content = Some(JArray(v)))
                  }
                }
              }
            }
          }
        }
      } ->
      shutdown { demoConfig: BlueEyesDemoConfig =>
        ().future
      }
    }
  }

  private def searchContacts(filterJObject: Option[JObject], config: BlueEyesDemoConfig): Future[List[JString]] = {
    createFilter(filterJObject) map { filter =>
      config.database(select().from(config.collection).where(filter)) map {
       _.toList.flatMap(_ \ "name" as JString)
      }
    } getOrElse {
      Future.sync[List[JString]](Nil)
    }
  }

  private def createFilter(obj: Option[JObject]) = obj.map {
    _.flatten.collect {
      case JObject(fields) => fields
    }.flatten.foldLeft[MongoFilter](MongoFilterAll) { (filter, field) =>
      filter && field.name === ((field.value as JString).map(_.value))
    }
  }
}

case class BlueEyesDemoConfig(config: ConfigMap, mongo: Mongo){
  val database    = mongo.database(config("database"))
  val collection  = config("collection")
}
