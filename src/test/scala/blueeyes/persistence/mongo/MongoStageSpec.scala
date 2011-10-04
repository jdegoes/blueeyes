package blueeyes.persistence.mongo

import java.util.concurrent.TimeUnit.{MILLISECONDS}
import org.specs.{ScalaCheck, Specification}
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop._

import blueeyes.persistence.cache.ExpirationPolicy
import scalaz.Semigroup
import org.specs.util.TimeConversions._
import scala.util.Random
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import blueeyes.json.{JPath, JsonAST, Printer, ArbitraryJPath}
import akka.actor.{Actor}
import scalaz._
import Scalaz._

class MongoStageSpec extends Specification with ScalaCheck with MongoImplicits with ArbitraryMongo{

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

  "MongoStage" should{
    "store all updates" in {
      forAll{updates: List[(MongoFilter, MongoUpdate)] =>
        val mockMongo        = new MockMongo()
        val mockDatabase     = mockMongo.database( "mydb" )
        val collection       = "mycollection"

        val mongStage   = MongoStage(mockDatabase, MongoStageSettings(ExpirationPolicy(Some(100), Some(100), MILLISECONDS), 500))
        val actorsCount = 10
        val sendCount   = 20
        val actors      = Array.fill(actorsCount){
          val actor = Actor.actorOf(new MessageActor(mongStage, updates.map(v => (v._1 & collection, v._2)), sendCount))
          actor.start()
          actor
        }

        val start = System.currentTimeMillis
        val futures = Future(actors.map(actor => fromAkka[Unit](actor !!! ("Send", 100000)).toBlueEyes): _*)
        futures.value must eventually(200, 300.milliseconds) (beSomething)

        val flushFuture = mongStage.flushAll
        flushFuture.value must eventually (beSomething)

        val pass = updates.foldLeft(true){(result, filterAndUdate) =>
          result && {
            val update = filterAndUdate._2.asInstanceOf[UpdateFieldFunctions.IncF]

            val resultFuture = mockDatabase(select().from(collection).where(filterAndUdate._1))
            resultFuture.isDelivered must eventually (be(true))

            val objects  = resultFuture.value.get.toList
            val setValue = update.value.asInstanceOf[MongoPrimitiveInt].value
            val value    = update.path.extract(objects.head)
            value == JsonAST.JInt(setValue * sendCount * actorsCount)
          }
        }
        actors.foreach(_.stop())
        pass
      } must pass
    }
  }
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
      self.reply(())
    case _ =>
  }
}
