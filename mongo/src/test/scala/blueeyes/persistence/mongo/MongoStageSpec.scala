package blueeyes.persistence.mongo

import java.util.concurrent.TimeUnit.{MILLISECONDS}
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop._

import blueeyes.persistence.cache.ExpirationPolicy
import scalaz.Semigroup
import scala.util.Random

import blueeyes.bkka.AkkaDefaults
import blueeyes.akka_testing.FutureMatchers
import akka.actor.{Actor, Props}
import akka.dispatch.Future
import akka.pattern.ask
import akka.util.Timeout

import blueeyes.json._
import scalaz._
import Scalaz._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

class MongoStageSpec extends Specification with ScalaCheck with MongoImplicits with ArbitraryMongo with FutureMatchers with AkkaDefaults with RealMongoSpecSupport {
  implicit val queryTimeout = Timeout(10000)

  implicit val StringSemigroup = new Semigroup[MongoUpdate] {
    def append(v1: MongoUpdate, v2: => MongoUpdate) = v1 |+| v2
  }

  def getJPathElement = Gen.listOfN(Gen.choose(2, 4).sample.get, Gen.alphaChar).map(chars => new String(chars.toArray))
  def genJPath        = Gen.listOfN(Gen.choose(2, 4).sample.get, getJPathElement).map(_.mkString(".")).map(path => JPath(path))

  def genFieldFilter[Gen[MongoFieldFilter]] = for{
    path     <- genJPath
    value    <- genSimple
  } yield {MongoFieldFilter(path, MongoFilterOperators.$eq, value)}


  def genUpdate: Gen[(MongoFilter, MongoUpdate)] = for{
    filters     <- Gen.listOfN(Gen.choose(3, 7).sample.get, genFieldFilter)
    updateValue <- Gen.choose(5, 10)
    path        <- genJPath
  } yield((MongoAndFilter(filters), path inc (updateValue)))

  implicit def arbUpdates = Arbitrary{Gen.listOfN(Gen.choose(1, 1).sample.get, genUpdate)}

  include (
    "MongoStage" should{
      "store all updates" in {
        //check { updates: List[(MongoFilter, MongoUpdate)] =>
        val updates = List(genUpdate.sample.get)
        val database         = mongo.database( "mydb" )
        val collection       = "mycollection"

        val mongoStage   = MongoStage(database, MongoStageSettings(ExpirationPolicy(Some(100), Some(100), MILLISECONDS), 500, Timeout(1000)))
        val actorsCount = 10
        val sendCount   = 20
        val actors      = List.fill(actorsCount) {
          defaultActorSystem.actorOf(Props(new MessageActor(mongoStage, updates.map(v => (v._1 & collection, v._2)), sendCount)))
        }

        val start = System.currentTimeMillis
        val futures = Future.sequence[Unit, List](actors.map(actor => (actor ? "Send").mapTo[Unit]))
        futures.value must eventually(200, 300.milliseconds) (beSome)

        val flushFuture = mongoStage.flushAll(Timeout(10000))
        flushFuture.value must (eventually(beSome[Either[Throwable, Int]]))

        forall(updates) {
          case (filter, update: UpdateFieldFunctions.IncF) =>
            database(select().from(collection).where(filter)).map(_.toList) must whenDelivered {
              beLike {
                  // FIXME: This ends up with Nil, because the select is returning nothing...
                case x :: xs =>
                  val setValue = update.value.asInstanceOf[MongoPrimitiveInt].value
                  val value    = update.path.extract(x)
                  value must_== JNum(setValue * sendCount * actorsCount)
              }
            }
        }

        //pass
        //}
      }.pendingUntilFixed
    }
  )
}

class MessageActor(stage: MongoStage, updates: List[(MongoFilterCollection, MongoUpdate)], size: Int) extends Actor{
  private val random    = new Random()
  def receive = {
    case "Send" =>
      for (j <- 0 until size){
        updates foreach { update =>
          stage.put(update._1, update._2)
        }
      }

      sender ! ()
  }
}
