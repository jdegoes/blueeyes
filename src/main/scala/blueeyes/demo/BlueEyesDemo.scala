package blueeyes.demo

import net.lag.configgy.ConfigMap
import blueeyes.BlueEyesServer
import blueeyes.BlueEyesServiceBuilder
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import blueeyes.core.http.{HttpHeaders}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.http.MimeTypes._
import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo.MongoImplicits._
import blueeyes.persistence.mongo.{ConfigurableMongo, MongoFilterAll, Mongo, MongoFilter}
import blueeyes.core.service.ServerHealthMonitorService
import blueeyes.core.http.{HttpStatusCodes, HttpStatus, HttpRequest, HttpResponse}
import blueeyes.core.data.FileSource._
import blueeyes.core.data.{FileSource, ByteChunk, BijectionsChunkJson, BijectionsChunkString, BijectionsChunkFutureJson}
import java.io.File
import blueeyes.health.metrics._

object BlueEyesDemo extends BlueEyesServer with BlueEyesDemoService with ServerHealthMonitorService{
  override def main(args: Array[String]) = super.main(Array("--configFile", "/etc/default/blueeyes.conf"))
}

trait BlueEyesDemoService extends BlueEyesServiceBuilder with HttpRequestCombinators with ConfigurableMongo{
  import BijectionsChunkJson._
  import BijectionsChunkString._
  import BijectionsChunkFutureJson._
  val contactListService = service("contactlist", "1.0.0") {
    requestLogging{
    help{
    healthMonitor { monitor => context =>
      startup {
        val mongoConfig = context.config.configMap("mongo")
        BlueEyesDemoConfig(mongoConfig, mongo(mongoConfig)).future
      } ->
      request { demoConfig: BlueEyesDemoConfig =>
        import demoConfig._

        describe("Gets all contacts."){
          path("/contacts"){
            produce(application/json) {
              get { request: HttpRequest[ByteChunk] =>
                database(select(".name").from(collection)) map { records =>
                  HttpResponse[JValue](
                    content = Some(JArray(records.map(_ \\ "name").toList))
                  )
                }
              }
            }
          }
        }~
        describe("Adds new contact."){
          path("/contacts"){
            jvalue {
              post { request: HttpRequest[Future[JValue]] =>
                request.content map {
                  _.flatMap(v => database(insert(v --> classOf[JObject]).into(collection)) map (_ => HttpResponse[JValue]()))
                } getOrElse {
                  Future.sync(HttpResponse[JValue](status = HttpStatus(BadRequest)))
                }
              }
            }
          }
        }~
        path("/contacts"){
          describe("Searchs contacts by criteria."){
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
          }~
          describe("Searches a conatact by name."){
            path("/'name") {
              produce(application/json) {
                get { request: HttpRequest[ByteChunk] =>
                  database(selectOne().from(collection).where("name" === request.parameters('name))) map {
                    v => HttpResponse[JValue](content=v, status=if (!v.isEmpty) OK else NotFound)
                  }
                }
              }
            }
          }~
          describe("Deletes a conatact by name."){
            path("/'name") {
              delete { request: HttpRequest[ByteChunk] =>
                database(remove.from(collection).where("name" === request.parameters('name))) map {
                  _ => HttpResponse[ByteChunk]()
                }
              }
            }
          }~
          describe("Reads a file."){
            path("/file/read"){
              compress{
                produce(image / jpeg){
                  get { request: HttpRequest[ByteChunk] =>
                    val response     = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = FileSource(new File("~/Downloads/victoria-parkside-resort.jpg")), headers = HttpHeaders.Empty + ("Content-Encoding", "gzip"))
                    Future.sync[HttpResponse[ByteChunk]](response)
                  }
                }
              }
            }
          }
        }~
        describe("Ping service."){
          path("/ping") {
            produce(text/plain) {
              get { request: HttpRequest[ByteChunk] =>
                Future.sync(HttpResponse[JValue](content = Some(JBool(true))))
              }
            }
          }
        }~
        describe("Jsonp service."){
          path("/Jsonp") {
            jsonp { request: HttpRequest[Future[JValue]] =>
              Future.sync(HttpResponse[JValue](content = Some(JBool(true))))
            }
          }
        }
      }->
      shutdown { demoConfig: BlueEyesDemoConfig =>
        ().future
      }
    }}}}

  private def searchContacts(filterJObject: Option[Future[JObject]], config: BlueEyesDemoConfig): Future[List[JString]] = {
    createFilter(filterJObject) map { _.flatMap{filter =>
        config.database(select().from(config.collection).where(filter)) map {
         _.toList.flatMap(_ \ "name" -->? classOf[JString])
        }
      }
    } getOrElse {
      Future.sync[List[JString]](Nil)
    }
  }

  private def createFilter(filterJObject: Option[Future[JObject]]) = filterJObject.map {_.map{
    _.flatten.collect {
      case f @ JField(_, JString(_)) => f
    }.foldLeft(MongoFilterAll.asInstanceOf[MongoFilter]) { (filter, field) =>
      filter && field.name === field.value.asInstanceOf[JString].value
    }
  }}
}

case class BlueEyesDemoConfig(config: ConfigMap, mongo: Mongo){
  val database    = mongo.database(config("database"))
  val collection  = config("collection")
}
