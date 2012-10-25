package blueeyes.persistence.mongo

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop._

import MongoFilterOperators._
import blueeyes.json._
import blueeyes.json._
import MongoFilterImplicits._
import akka.dispatch.Future
import scalaz._
import Scalaz._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.specs2.matcher.ThrownMessages
import akka.util.Timeout

class MongoPatchesSpec extends Specification with ScalaCheck with MongoImplicits with ArbitraryJValue with ArbitraryMongo with blueeyes.bkka.AkkaDefaults with ThrownMessages {
  implicit val queryTimeout = Timeout(10 * 60 * 1000)

  def getPatch = for{
    filter <- getMongoFieldFilter
    update <- Gen.containerOfN[List, MongoUpdate](Gen.choose(1, 3).sample.get, getMongoUpdate)
  } yield ((filter, update))

  def getListPath = Gen.containerOfN[List, (MongoFilter, List[MongoUpdate])](Gen.choose(2, 3).sample.get, getPatch)
  def getTwoListPath = for (list1 <-getListPath; list2 <- getListPath) yield((list1, list2))

  implicit def arbListPatch: Arbitrary[List[(MongoFilter, List[MongoUpdate])]] = Arbitrary(getListPath)
  implicit def arbTwoListPatch: Arbitrary[(List[(MongoFilter, List[MongoUpdate])], List[(MongoFilter, List[MongoUpdate])])] = Arbitrary(getTwoListPath)

  // ======================================================================
  // This spec is disabled because it relies on a mock mongo. Unclear how to make it work on a real mongo
  // ======================================================================


  //"MongoPatches" should{
    //"commit all patches when patch is added one by one" in{
    //  check{ patches: List[(MongoFilter, List[MongoUpdate])] =>
    //    checkCommit(patches, createMongoPatches(patches))
    //  }
    //}
    //"commit all patches when patches are merged" in{
    //  check{ patches: (List[(MongoFilter, List[MongoUpdate])], List[(MongoFilter, List[MongoUpdate])]) =>
    //    checkCommit(patches._1 ::: patches._2, createMongoPatches(patches._1) ++ createMongoPatches(patches._2))
    //  }
    //}
  //}

//  def createMongoPatches(patches: List[(MongoFilter, List[MongoUpdate])]) = {
//    patches.foldLeft(MongoPatches.empty) { (mongoPatch, patches) => patches._2.foldLeft(mongoPatch){ (mongoPatch, update) => mongoPatch + (patches._1, update)}}
//  }
//
//  def checkCommit(patches: List[(MongoFilter, List[MongoUpdate])], mongoPatches: MongoPatches) = {
//    val database     = new MongoDatabaseImpl(patches.map(v => (v._1, v._2.suml)))
//
//    mongoPatches.commit(database, "foo")
//
//    database.queries.isEmpty
//  }
//
//  class MongoDatabaseImpl(var queries: List[(MongoFilter, MongoUpdate)]) extends Database {
//    override def apply[T <: MongoQuery](query: T)(implicit m: Manifest[T#QueryResult], timeout: akka.util.Timeout) = {
//      val update = query.asInstanceOf[MongoUpdateQuery]
//      queries = queries filterNot (_ == (update.filter.get, update.value))
//
//      akka.dispatch.Promise.failed[T#QueryResult](new RuntimeException("Should not be seen."))
//    }
//  }
}
