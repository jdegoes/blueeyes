package blueeyes.persistence.mongo

import scala.annotation.tailrec

import MongoFilterOperators._

import blueeyes.json._
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

  "Mongo" should{
    skip("run manually")
    "Select the same value form Mock and Real Mongo for And operator" in{
      forAll { value: JObject =>

        query[JNothing.type](insert(value).into(collection))
        val pass = checkSelectPass(value)
        query[JNothing.type](remove.from(collection))

        pass
      } must pass
    }
  }

  private def checkSelectPass(value: JObject): Boolean = {
    val selectQuery  = generateQuery(value.flattenWithPath)
    val selection    = query(select().from(collection).where(selectQuery))
    val real         = selection._1.toList
    val mock         = selection._2.toList
    val pass         = real == mock
    if (!pass) {
      println("--------OBJECT--------")
      println(Printer.compact(JsonAST.render(value)))
      println("--------OBJECT--------")
      println("--------QUERY--------")
      println(Printer.compact(JsonAST.render(selectQuery.filter)))
      println("--------QUERY--------")
      oneQuery(select().from(collection).where(selectQuery), mockDatabase)
    }
    pass
  }

  private def query[T](query: MongoQuery[T]): (T, T) = (oneQuery(query, realDatabase), oneQuery(query, mockDatabase))
  private def oneQuery[T](query: MongoQuery[T], database: MongoDatabase) = {
    val future = database(query)

    future.isDelivered must eventually (be(true))

    future.value.get
  }

  private def generateQuery(values: List[(JPath, JValue)]) = {
    val filters = values.foldLeft(List[MongoFieldFilter]()) {
      (result, element) => filter(element) :: result
    } filterNot { filter => filter.operator == $exists && filter.lhs.nodes.last.isInstanceOf[JPathIndex]}
    MongoAndFilter(filters)
  }

  private def filter(pathAndValue: (JPath, JValue)) = {
    pathAndValue._2 match{
      case e: JInt    => filterForInt(pathAndValue._1, e)
      case e: JDouble => filterForDouble(pathAndValue._1, e)
      case e: JBool   => filterForBoolean(pathAndValue._1, e)
      case e: JString => filterForString(pathAndValue._1, e)
      case e: JArray  => filterForArray(pathAndValue._1, e)
      case JNull      => filterForNull(pathAndValue._1)
      case _          => MongoFieldFilter(pathAndValue._1, $eq, pathAndValue._2)
    }
  }

  def filterForNull(path: JPath)                    = Gen.oneOf[MongoFieldFilter](path.hasType[JNull.type], path === JNull).sample.get
  def filterForInt(path: JPath, value: JInt)        = Gen.oneOf[MongoFieldFilter](path.hasType[JDouble], path.isDefined, path !== JNull, path === value, path !== JInt(value.value - 1), path > JInt(value.value - 1), path >= JInt(value.value - 1), path < JInt(value.value + 1), path <= JInt(value.value + 1), path.anyOf(MongoPrimitiveInt(value.value.toInt))).sample.get
  def filterForString(path: JPath, value: JString)  = Gen.oneOf[MongoFieldFilter](path.hasType[JString], path.isDefined, path !== JNull, path === value, path !== JString(value.value + "a"), path.anyOf(MongoPrimitiveString(value.value))).sample.get
  def filterForArray(path: JPath, value: JArray)    = Gen.oneOf[MongoFieldFilter](path.hasType[JArray], path.isDefined, path !== JNull, path === value, path !== JArray(JString("a") :: value.elements), path.hasSize(value.elements.length), path.contains[MongoPrimitive[_]](value.elements.map(jvalueToMongoPrimitive(_).get): _*)).sample.get
  def filterForDouble(path: JPath, value: JDouble)  = Gen.oneOf[MongoFieldFilter](path.hasType[JDouble], path.isDefined, path !== JNull, path === value, path !== JDouble(value.value - 1), path > JDouble(value.value - 1), path >= JDouble(value.value - 1), path < JDouble(value.value + 1), path <= JDouble(value.value + 1), path.anyOf(MongoPrimitiveDouble(value.value))).sample.get
  def filterForBoolean(path: JPath, value: JBool)   = Gen.oneOf[MongoFieldFilter](path.hasType[JBool], path.isDefined, path !== JNull, path === value, path !== JBool(!value.value), path.anyOf(MongoPrimitiveBoolean(value.value))).sample.get
}