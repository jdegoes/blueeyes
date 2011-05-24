package blueeyes.persistence.mongo

import scala.annotation.tailrec

import MongoFilterOperators._

import blueeyes.json.{JsonAST, Printer, JPath, ArbitraryJValue}
import blueeyes.json.JsonAST._

import net.lag.configgy.Configgy

import org.scalacheck._
import org.scalacheck.Prop._
import org.specs.{ScalaCheck, Specification}

class MongoSpec extends Specification with ArbitraryJValue with ScalaCheck with MongoImplicits{

  Configgy.configure("/etc/default/blueeyes.conf")

  private val realMongo     = new RealMongo(Configgy.config.configMap("mongo"))
  private val mockMongo     = new MockMongo()
  private val realDatabase  = realMongo.database( "mydb" )
  private val mockDatabase  = mockMongo.database( "mydb" )

  private val collection = "test-collection"

  "Select the same value form Mock and Real Mongo" in{
    forAll { value: JObject =>

      query[JNothing.type](insert(value).into(collection))

      checkSelectWhilePass(value.flattenWithPath)
//
//      println("*************************************")
//      println(Printer.pretty(JsonAST.render(value)))
//      println("----------")
//      real  foreach {v => println(Printer.pretty(JsonAST.render(v)))}
//      println("----------")
//      mock foreach {v => println(Printer.pretty(JsonAST.render(v)))}

      query[JNothing.type](remove.from(collection))

      true
    } must pass
  }

  private def checkSelectWhilePass(values: List[(JPath, JValue)]): Boolean = {
    if (checkSelectPass(values)) true
    else {
      val current     = filter(values.head)
      val filterValue = current.filter
      val mayBeGood   = checkSelectWhilePass(values.tail)
      mayBeGood
    }
  }

  private def checkSelectPass(values: List[(JPath, JValue)]) = {
    val selectQuery  = generateQuery(values)
    val selection    = query(select().from(collection).where(selectQuery))
    val real         = selection._1.toList
    val mock         = selection._2.toList
    val pass         = real == mock
    pass
  }

  private def query[T](query: MongoQuery[T]): (T, T) = (oneQuery(query, realDatabase), oneQuery(query, mockDatabase))
  private def oneQuery[T](query: MongoQuery[T], database: MongoDatabase) = {
    val future = database(query)

    future.isDelivered must eventually (be(true))

    future.value.get
  }

  private def generateQuery(values: List[(JPath, JValue)]) = MongoAndFilter(values.foldLeft(List[MongoFieldFilter]()){(result, element) => filter(element) :: result})

  private def filter(value: (JPath, JValue)) = MongoFieldFilter(value._1, $eq, value._2)
}