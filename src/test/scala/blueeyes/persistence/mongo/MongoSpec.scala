package blueeyes.persistence.mongo

import scala.annotation.tailrec

import MongoFilterOperators._

import blueeyes.json.{JsonAST, Printer, JPath, ArbitraryJValue}
import blueeyes.json.JsonAST._

import net.lag.configgy.Configgy

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop._
import org.specs.{ScalaCheck, Specification}

class MongoSpec extends Specification with ArbitraryJValue with ScalaCheck with MongoImplicits{

  Configgy.configure("/etc/default/blueeyes.conf")

  private val realMongo     = new RealMongo(Configgy.config.configMap("mongo"))
  private val mockMongo     = new MockMongo()
  private val realDatabase  = realMongo.database( "mydb" )
  private val mockDatabase  = mockMongo.database( "mydb" )

  private val collection = "test-collection"

//  "Select the same value form Mock and Real Mongo" in{
//    forAll { value: JObject =>
//
//        println("-----------------")
//        println(Printer.compact(JsonAST.render(value)))
//      query[JNothing.type](insert(value).into(collection))
//
//      val pass = checkSelectWhilePass(value.flattenWithPath)
//
//      query[JNothing.type](remove.from(collection))
//
//      pass
//    } must pass
//  }

  private def checkSelectWhilePass(values: List[(JPath, JValue)]): Boolean = {
    println("-----------------")
    println(Printer.compact(JsonAST.render(generateQuery(values).filter)))
    if (checkSelectPass(values)) true
    else {
      val current     = filter(values.head)
      val filterValue = current.filter
      println(Printer.compact(JsonAST.render(filterValue)))
      val mayBeGood   = checkSelectWhilePass(values.tail)
      mayBeGood
    }
  }

  private def checkSelectPass(values: List[(JPath, JValue)]): Boolean = {
    val selectQuery  = generateQuery(values)
    val selection    = query(select().from(collection).where(selectQuery))
    val real         = selection._1.toList
    val mock         = selection._2.toList
    val pass         = real == mock
    val mayBePass = if (pass) true else {
      checkSelectPass(values.tail)
    }
    pass
  }

  private def query[T](query: MongoQuery[T]): (T, T) = (oneQuery(query, realDatabase), oneQuery(query, mockDatabase))
  private def oneQuery[T](query: MongoQuery[T], database: MongoDatabase) = {
    val future = database(query)

    future.isDelivered must eventually (be(true))

    future.value.get
  }

  private def generateQuery(values: List[(JPath, JValue)]) = MongoAndFilter(values.foldLeft(List[MongoFieldFilter]()){(result, element) => filter(element) :: result})

  private def filter(pathAndValue: (JPath, JValue)) = {
    pathAndValue._2 match{
      case e: JInt    => filterForInt(pathAndValue._1, e)
      case e: JDouble => filterForDouble(pathAndValue._1, e)
      case e: JBool   => filterForBoolean(pathAndValue._1, e)
      case _          => MongoFieldFilter(pathAndValue._1, $eq, pathAndValue._2)
    }
  }

  def filterForInt(path: JPath, value: JInt)        = Gen.oneOf[MongoFieldFilter](path.hasType[JDouble], path.isDefined, path !== JNull, path === value, path !== JInt(value.value - 1), path > JInt(value.value - 1), path >= JInt(value.value - 1), path < JInt(value.value + 1), path <= JInt(value.value + 1)).sample.get
  def filterForDouble(path: JPath, value: JDouble)  = Gen.oneOf[MongoFieldFilter](path.hasType[JDouble], path.isDefined, path !== JNull, path === value, path !== JDouble(value.value - 1), path > JDouble(value.value - 1), path >= JDouble(value.value - 1), path < JDouble(value.value + 1), path <= JDouble(value.value + 1)).sample.get
  def filterForBoolean(path: JPath, value: JBool)   = Gen.oneOf[MongoFieldFilter](path.hasType[JBool], path.isDefined, path !== JNull, path === value, path !== JBool(!value.value)).sample.get
}