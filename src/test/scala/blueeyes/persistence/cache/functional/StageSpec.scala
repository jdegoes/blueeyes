package blueeyes
package persistence.cache.functional

import org.specs.Specification
import org.specs.ScalaCheck
import org.specs.specification.PendingUntilFixed
import java.util.concurrent.TimeUnit.{MILLISECONDS}

import scala.util.Random
import scala.collection.immutable.ListMap
import scalaz._
import scalaz.Scalaz._
import blueeyes.concurrent.{Future, FutureDeliveryStrategySequential}
import blueeyes.concurrent.FutureImplicits._

import scala.collection.mutable.ArrayBuilder.ofRef

import org.scalacheck.Prop._
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Shrink

class StageSpec extends Specification with ScalaCheck { //with org.specs.runner.ScalaTest {
  val BigStage = Stage.empty[Int, String](Int.MaxValue / 10)
  val SmallStage = Stage.empty[Int, String](2, 4)

  implicit val arbString = Arbitrary[String](identifier)

  implicit val arbStageIn = Arbitrary[StageIn[Int, String]] {
    val genPutAll: Gen[StageIn[Int, String]] = {
      for {
        iter <- arbitrary[List[(Int, String)]] 
        time <- choose(100, 200)
      } yield PutAll(iter, time)
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

  implicit val stageShrink = Shrink[StageIn[Int, String]] {
    case PutAll(iter, time) => 
      implicitly[Shrink[List[(Int, String)]]].shrink(iter.toList).map(PutAll(_, time))

    case other => Stream.empty
  }

  def reduceMap[K, V](m: Iterable[(K, V)])(implicit semigroup: Semigroup[V]): ListMap[K, V] = m.foldLeft(ListMap.empty[K, V]) {
    case (m, (k, v)) => m + (k -> m.get(k).map(semigroup.append(_, v)).getOrElse(v))
  }

  "information content" should {
    "be invariant" in {

      forAll { (operations: List[StageIn[Int, String]], stage: Stage[Int, String]) => 
        val (discarded, finalStage) = operations.foldLeft((Map.empty[Int, String], stage)) { 
          case ((discarded1, stage), operation) =>
            val (discarded2, stage2) = stage.apply(operation) 

            (discarded1 <+> discarded2, stage2)
        }

        val (finalFlushed, _) = finalStage(ExpireAll)

        val expectedDiscarded = (operations.foldLeft(Map.empty[Int, String]) {
          case (result, PutAll(iter: Iterable[(Int, String)], _)) => iter.foldLeft(result) {
            case (result, (k, v2)) => 
              result + (k -> result.get(k).map(v1 => implicitly[Semigroup[String]].append(v1, v2)).getOrElse(v2))
            }
          case (result, _) => result
        })

        val totalPuts = operations.collect { 
          case PutAll(iter: Iterable[(Int, String)], _) => iter.size
        }.sum

        (discarded <+> finalFlushed) == expectedDiscarded
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
      "evict" in {
        import scala.collection.immutable.ListMap

        forAll { (toAdd: Map[Int, String]) =>
          val (expired, nextStage) = SmallStage.putAll(toAdd, 100)

          if (toAdd.size > SmallStage.maxCapacity) {
            expired.size ==  toAdd.size - SmallStage.baseCapacity
          } else {
            expired.isEmpty
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

        expired == toAdd
      } must pass
    }
  }

  case class TimedInput(data: Map[Int, String], time: Long)

  implicit val tiGen = Arbitrary[TimedInput] {
    for {
      l <- arbitrary[Map[Int, String]] if !l.isEmpty
      time <- choose(100L, 200L)
    } yield TimedInput(l, time)
  }
  
  case class AllTimedInput(list: List[TimedInput], cutoff: Long)

  implicit val arbAllTimedInput = Arbitrary[AllTimedInput] {
    for {
      list   <- arbitrary[List[TimedInput]]
      cutoff <- choose(100L, 201L)
    } yield AllTimedInput(list, cutoff)
  }

  implicit val TimedInputShrink= Shrink[TimedInput] {
    case TimedInput(data, time) =>
      implicitly[Shrink[List[(Int, String)]]].shrink(data.toList).map(s => TimedInput(s.toMap, time))
  }

  implicit val AllTimedInputShrink = Shrink[AllTimedInput] {
    case AllTimedInput(list, cutoff) =>
      implicitly[Shrink[List[TimedInput]]].shrink(list).map(AllTimedInput(_, cutoff))
  }

  "expire" should {
    "evict by a specific access time" in {
      val input1 = PutAll(Map(0 -> ""), 145)
      val input2 = PutAll(Map(0 -> ""), 102)

      val (out1, stage1) = BigStage.putAll(input1.values, input1.time)
      out1 must beEmpty

      val (out2, stage2) = stage1.putAll(input2.values, input2.time)
      out2 must beEmpty

      val (out3, stage3) = stage2.expire(0, 110)
      out3 must beEmpty

      val (out4, stage4) = stage3.expireAll
      out4 must_== Map(0 -> "")
    }

    "evict by access time" in {
      forAll { (input: AllTimedInput) => 
        val AllTimedInput(list, accessTimeCutoff) = input
        val sortedList = list.sortBy(_.time)

        val stage = sortedList.foldLeft(BigStage) {
          case (stage, TimedInput(toPut, time)) => stage.putAll(toPut, time)._2
        }

        val (expectedRemoved, expectedRetained) = ToTuple2W(sortedList.partition(_.time < accessTimeCutoff)).fold {
          case (before, after) => 
            val retained = SeqMA(after.map(_.data)).sum
            val (removedRetained, removed) = SeqMA(before.map(_.data)).sum.partition {
              case (k, _) => retained.contains(k)
            }

            (removed, removedRetained <+> retained)
        }

        val (removed, nextStage) = stage.expire(0, accessTimeCutoff)

        val retained = nextStage.expireAll._1

        (removed  == expectedRemoved)  :| ("Expected removed: " + expectedRemoved  + " but found " + removed) && 
        (retained == expectedRetained) :| ("Expected retained: " + expectedRetained + " but found " + retained)
      } must pass
    }

    "evict by creation time" in {
      forAll { (input: AllTimedInput) => 
        val AllTimedInput(list, creationTimeCutoff) = input
        val sortedList = list.sortBy(_.time)

        val stage = sortedList.foldLeft(BigStage) {
          case (stage, TimedInput(toPut, time)) => stage.putAll(toPut, time)._2
        }

        val (expectedRemoved, expectedRetained) = ToTuple2W(sortedList.partition(_.time < creationTimeCutoff)).fold {
          case (before, after) => 
            val removed = SeqMA(before.map(_.data)).sum
            val (retainedRemoved, retained) = SeqMA(after.map(_.data)).sum.partition {
              case (k, _) => removed.contains(k)
            }

            (removed <+> retainedRemoved, retained)
        }

        val (removed, nextStage) = stage.expire(creationTimeCutoff, 0)

        val retained = nextStage.expireAll._1

        (removed  == expectedRemoved)  :| ("Expected removed: " + expectedRemoved  + " but found " + removed) && 
        (retained == expectedRetained) :| ("Expected retained: " + expectedRetained + " but found " + retained)
      } must pass
    }
  }

}
