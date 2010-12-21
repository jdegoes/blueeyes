package blueeyes.demo

import blueeyes.core.service.engines.HttpClientXLightWebEnginesArrayByte
import blueeyes.core.http.MimeTypes._
import net.lag.configgy.Configgy
import blueeyes.core.http.HttpResponse
import blueeyes.BlueEyesClientTransformerBuilder
import blueeyes.json.JsonAST.{JString, JArray, JValue}
import java.util.concurrent.CountDownLatch
import blueeyes.util.Future
import Serialization._
import blueeyes.core.service.HttpClient

object BlueEyesClientDemo extends HttpClientXLightWebEnginesArrayByte{
  private var clientFacade: ContactListFacade = _
  private var client: HttpClient[Array[Byte]] = _
  def main(args: Array[String]){

    client        = new HttpClientXLightWebEnginesArrayByte{}
    clientFacade  = createClientFacade

    ->?(clientFacade.createContact(contact))

    ->?(clientFacade.contacts) foreach println

    ->?(clientFacade.contact(contact.name)) foreach println
  }

  private def contact = Contact("John", Some("john@google.com"), Some("UK"), Some("London"), None)

  private def createClientFacade = {
    Configgy.configure("/etc/default/blueeyes.conf")

    val port = Configgy.config.configMap("server").getInt("port", 8888)

    new ContactListFacade(port)    
  }

  private def ->?[T](f: HttpClient[Array[Byte]] => Future[T]) = {

    val future    = client.exec(f) 
    val counDown  = new CountDownLatch(1)

    future.deliverTo(response =>{
      counDown.countDown
    })
    counDown.await
    future.value.get
  }
}

class ContactListFacade(port: Int) extends BlueEyesClientTransformerBuilder{
  def contacts =  protocol("http"){
    host("localhost"){
      port(port){
        path("/contacts"){
          contentType[JValue, Array[Byte], List[String]](application/json){
            get[JValue, List[String]]{response: HttpResponse[JValue] =>
              response.content match{
                case Some(e: JArray) => e.elements.map(v => {
                  v match{
                    case JString(x) => x
                    case _ => error("wrong type")
                  }
                })
                case _ => Nil
              }
            }
          }
        }
      }
    }
  }

  def contact(name: String) =  protocol("http"){
    host("localhost"){
      port(port){
        path("/contacts/" + name){
          contentType[JValue, Array[Byte], Option[Contact]](application/json){
            get[JValue, Option[Contact]]{response: HttpResponse[JValue] =>
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

  def createContact(contact: Contact)  = protocol("http"){
    host("localhost"){
      port(port){
        path("/contacts"){
          contentType[JValue, Array[Byte], Option[JValue]](application/json){
            post[JValue, Option[JValue]](contact.serialize){response: HttpResponse[JValue] => response.content}
          }
        }
      }
    }
  }
}