package blueeyes.demo

import blueeyes.core.service.engines.HttpClientXLightWebEngines
import blueeyes.core.http.MimeTypes._
import net.lag.configgy.Configgy
import blueeyes.core.http.HttpResponse
import blueeyes.json.JsonAST._
import java.util.concurrent.CountDownLatch
import blueeyes.concurrent.Future
import Serialization._
import blueeyes.core.service.HttpClient
import blueeyes.json.JsonParser.{parse => j}
import blueeyes.core.data.{BijectionsChunkJson, ByteChunk, Bijection}

object BlueEyesClientDemo extends BlueEyesDemoFacade  with Data{

  Configgy.configure("/etc/default/blueeyes.conf")

  val port = Configgy.config.configMap("server").getInt("port", 8888)

  val httpClient = new HttpClientXLightWebEngines{}

  def main(args: Array[String]){

    ->?(create(contact))

    ->?(list) foreach println

    ->?(search(j("""{ "name" : "%s" }""".format(contact.name)))) foreach println

    ->?(contact(contact.name)) foreach println

    ->?(remove(contact.name))

    ->?(list) foreach println
  }

  private def ->?[T](future: Future[T]) = {

    val counDown  = new CountDownLatch(1)

    future.deliverTo(response =>{
      counDown.countDown
    })
    counDown.await
    future.value.get
  }
}

trait BlueEyesDemoFacade extends BijectionsChunkJson{

  private implicit val jvalaueToJValue = Bijection.identity[JValue]

  def httpClient: HttpClient[ByteChunk]

  def port: Int

  private def jsonHttpClient = httpClient.contentType[JValue](application/json).protocol("http").port(port)

  def create(contact: Contact)  = jsonHttpClient.post("/contacts")(contact.serialize).map(_.content)

  def health()  = jsonHttpClient.get("/blueeyes/services/contactlist/v1/health").map(_.content)

  def list = jsonHttpClient.get("/contacts").map(response => namesFromJValue(response.content))

  def search(filter: JValue) = jsonHttpClient.post("/contacts/search")(filter).map(response => namesFromJValue(response.content))

  def contact(name: String) =  jsonHttpClient.get("/contacts/" + name).map[Option[Contact]] {response: HttpResponse[JValue] =>
    response.content match{
      case Some(x) => Some(x.deserialize[Contact])
      case _ => None
    }
  }

  def remove(name: String) = jsonHttpClient.delete("/contacts/" + name).map(_.content)

  private def namesFromJValue(jValue: Option[JValue]) = jValue match{
    case Some(e: JArray) => e.elements.map(v => {
      v match{
        case JString(x) => x
        case _ => error("wrong type")
      }
    })
    case _ => Nil
  }

}

trait Data{
  val contact = Contact("John", Some("john@google.com"), Some("UK"), Some("London"), None)
}