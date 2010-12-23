package blueeyes.demo

import blueeyes.core.service.engines.HttpClientXLightWebEnginesArrayByte
import blueeyes.core.http.MimeTypes._
import net.lag.configgy.Configgy
import blueeyes.core.http.HttpResponse
import blueeyes.BlueEyesClientTransformerBuilder
import blueeyes.json.JsonAST._
import java.util.concurrent.CountDownLatch
import blueeyes.util.Future
import Serialization._
import blueeyes.core.service.HttpClient
import blueeyes.json.JsonParser.{parse => j}

object BlueEyesClientDemo extends ContactListFacade with HttpClientXLightWebEnginesArrayByte with Data{

  Configgy.configure("/etc/default/blueeyes.conf")

  val port = Configgy.config.configMap("server").getInt("port", 8888)

  def main(args: Array[String]){

    ->?(create(contact))

    ->?(list) foreach println

    ->?(search(j("""{ "name" : "%s" }""".format(contact.name)))) foreach println

    ->?(contact(contact.name)) foreach println

    ->?(remove(contact.name))

    ->?(list) foreach println
  }

  private def ->?[T](f: HttpClient[Array[Byte]] => Future[T]) = {

    val future    = exec(f) 
    val counDown  = new CountDownLatch(1)

    future.deliverTo(response =>{
      counDown.countDown
    })
    counDown.await
    future.value.get
  }
}

trait ContactListFacade extends BlueEyesClientTransformerBuilder{

  def port: Int

  def create(contact: Contact)  = protocol$("http"){
    host$("localhost"){
      port$(port){
        path$("/contacts"){
          contentType$[JValue, Array[Byte], Option[JValue]](application/json){
            post$[JValue, Option[JValue]](contact.serialize){response: HttpResponse[JValue] =>
              response.content
            }
          }
        }
      }
    }
  }

  def list =  protocol$("http"){
    host$("localhost"){
      port$(port){
        path$("/contacts"){
          contentType$[JValue, Array[Byte], List[String]](application/json){
            get$[JValue, List[String]]{response: HttpResponse[JValue] =>
              namesFromJValue(response.content)
            }
          }
        }
      }
    }
  }

  def search(filter: JValue) =  protocol$("http"){
    host$("localhost"){
      port$(port){
        path$("/contacts/search"){
          contentType$[JValue, Array[Byte], List[String]](application/json){
            post$[JValue, List[String]](filter){response: HttpResponse[JValue] =>
              namesFromJValue(response.content)
            }
          }
        }
      }
    }
  }

  def contact(name: String) =  protocol$("http"){
    host$("localhost"){
      port$(port){
        path$("/contacts/" + name){
          contentType$[JValue, Array[Byte], Option[Contact]](application/json){
            get$[JValue, Option[Contact]]{response: HttpResponse[JValue] =>
              response.content match{
                case Some(x) => Some(x.deserialize[Contact]) 
                case _ => None
              }
            }
          }
        }
      }
    }
  }

  def remove(name: String)  = protocol$("http"){
    host$("localhost"){
      port$(port){
        path$("/contacts/" + name){
          contentType$[JValue, Array[Byte], Option[JValue]](application/json){
            delete$[JValue, Option[JValue]]{response: HttpResponse[JValue] => response.content}
          }
        }
      }
    }
  }

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