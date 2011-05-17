package blueeyes
package persistence.cache.functional

import org.specs.Specification
import org.specs.ScalaCheck
import org.specs.specification.PendingUntilFixed
import java.util.concurrent.TimeUnit.{MILLISECONDS}
import java.util.concurrent.CountDownLatch

import scala.util.Random
import scala.collection.immutable.ListMap
import scalaz.Semigroup
import scalaz.Scalaz._
import blueeyes.concurrent.{ActorStrategy, Actor, Future}
import scala.collection.mutable.ArrayBuilder.ofRef
import ActorStrategy._

import org.scalacheck.Prop.{forAll}
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Shrink

class StageSpec extends Specification with ScalaCheck with PendingUntilFixed {
  val BigStage = Stage.empty[Int, String](Int.MaxValue / 10)
  val SmallStage = Stage.empty[Int, String](2, 4)

  implicit val arbString = Arbitrary[String](identifier)

  implicit val arbStageIn = Arbitrary[StageIn[Int, String]] {
    val genPutAll: Gen[StageIn[Int, String]] = {
      for (iter <- arbitrary[List[(Int, String)]]; time <- choose(100, 200)) 
        yield PutAll(iter, time)
    }

    val genExpireAll: Gen[StageIn[Int, String]] = Gen.value(ExpireAll)

    val genExpire: Gen[StageIn[Int, String]] = {
      for (creationTime <- choose(100, 200); accessTime <- choose(100, 200)) 
        yield Expire(creationTime, accessTime)
    }
    
    Gen.oneOf(genPutAll, genExpireAll, genExpire)
  }

  implicit val arbStrage = Arbitrary[Stage[Int, String]] {
    for (base <- choose(2, 10); max <- choose(10, 18)) yield Stage.empty[Int, String](base, max)
  }

  implicit val stageShrinker = Shrink[StageIn[Int, String]] {
    case PutAll(iter, time) => 
      implicitly[Shrink[List[(Int, String)]]].shrink(iter.toList).map(PutAll(_, time))

    case other => Stream.empty
  }

  "information content" should {
    "be invariant" in {

      forAll { (operations: List[StageIn[Int, String]], stage: Stage[Int, String]) => 
        val (discarded, finalStage) = operations.foldLeft((Map.empty[Int, String], stage)) { 
          case ((discarded1, stage), operation) =>
            val (discarded2, stage2) = stage.apply(operation) 

            (MapMonoid[Int, String].append(discarded1, discarded2), stage2)
        }

        val (finalFlushed, _) = finalStage(ExpireAll)

        val expectedDiscarded = (operations.foldLeft(Map.empty[Int, String]) {
          case (result, PutAll(iter: Iterable[(Int, String)], _)) => iter.foldLeft(result) {
            case (result, (k, v2)) => 
              result + (k -> result.get(k).map(v1 => implicitly[Semigroup[String]].append(v1, v2)).getOrElse(v2))
            }
          case (result, _) => result
        })

        print(".")
        MapMonoid[Int, String].append(discarded, finalFlushed) must_== expectedDiscarded
      } must pass
    }
  }

  "putAll" should {
    "not evict added elements" in {
      forAll { (toAdd: Map[Int, String]) =>
        val (expired, nextStage) = BigStage.putAll(toAdd, 100)

        expired must beEmpty
      } must pass
    }

    "when adding to a stage over capacity" >> {
      "evict" in pendingUntilFixed {
        forAll { (toAdd: List[(Int, String)]) =>
          val (expired, nextStage) = SmallStage.putAll(toAdd, 100)
          if (toAdd.size > SmallStage.maxCapacity) {
            expired must haveSameElementsAs(toAdd.drop(toAdd.size - SmallStage.baseCapacity))
          } else {
            expired must beEmpty 
          }
        } must pass
      }
    }
  }

  "expireAll" should {
    "evict everything" in {
      forAll { (toAdd: Map[Int, String]) => 
        val (_, nextStage) = BigStage.putAll(toAdd, 100)

        val (expired, _) = nextStage.expireAll

        expired must_== toAdd
      } must pass
    }
  }

}
