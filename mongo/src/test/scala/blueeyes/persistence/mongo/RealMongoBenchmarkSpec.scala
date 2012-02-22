package blueeyes.persistence.mongo

import blueeyes.bkka.AkkaDefaults
import blueeyes.concurrent.test.FutureMatchers
import blueeyes.json._
import blueeyes.json.JsonAST._
import akka.dispatch.Future
import akka.dispatch.Future._

import akka.actor.Actor
import akka.actor.Props
import akka.dispatch.Dispatchers
import akka.pattern.ask
import akka.util.Duration
import akka.util.Timeout

import java.util.concurrent.TimeUnit
import org.scalacheck.Gen._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import org.streum.configrity.Configuration
import org.streum.configrity.io.BlockFormat

class RealMongoBenchmarkSpec extends Specification with ArbitraryJValue with MongoImplicits with ScalaCheck with FutureMatchers with AkkaDefaults {
  val testLive = (new java.io.File("/etc/default/blueeyes.conf")).exists
  val config = if (testLive) 
    Configuration.load("/etc/default/blueeyes.conf", BlockFormat) 
  else
    Configuration.parse("", BlockFormat)
  
  implicit val queryTimeout = Timeout(10000)

  private lazy val mongo  = RealMongo(config.detach("mongo"))
  private lazy val database  = mongo.database( "mydb" )

  private val collection    = "test-collection"

  override def is = args(skipAll = true) ^ super.is

  "Mongo" should {
    "insert multiple quiries concurrently" in{
      val start = System.currentTimeMillis

      val actors = List.range(1, 50) map {index =>
        defaultActorSystem.actorOf(Props(new MessageActor(alphaStr.sample.get, alphaStr.sample.get, index)))
      }
      val futures = Future.sequence(actors.map(actor => ((actor ? "send")(2000)).mapTo[Tuple3[Future[Option[JObject]], String, String]]))
      futures must whenDelivered {
        beLike {
          case list => forall(list) {
            case (selectFuture, name, value) => selectFuture.map(_.get \ name) must whenDelivered(be_==(JString(value)))
          }
        }
      }

      val removeFuture = database(remove.from(collection))
      val result = removeFuture.value must eventually (beSome)

      println("Execution time = " + (System.currentTimeMillis - start))

      result
    }
  }

  class MessageActor(name: String, value: String, index: Int) extends Actor{
    def receive = {
      case "send" => {
        val filter = "bucketId" === index && "entityId"    === index && "variationId" === index

        val insertFuture = database(upsert(collection).set(name set value).where(filter))
        val selectFuture = insertFuture.flatMap{v => database(selectOne().from(collection).where(filter))}

        sender ! ((selectFuture, name, value))
      }
      case _ =>
    }
  }
}

