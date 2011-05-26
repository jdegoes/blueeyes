package blueeyes.persistence.mongo

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
  private val collection    = "test-collection"

  def size = choose(20, 100).sample.get
  def genJObjects = Gen.containerOfN[List, JObject](size, genObject)
  implicit def arbJObjects: Arbitrary[List[JObject]] = Arbitrary(genJObjects)

  "Mongo" should{
    skip("run manually")
    "Select the same value form Mock and Real Mongo for And operator" in{
      forAll { values: List[JObject] =>
        query[JNothing.type](insert(values: _*).into(collection))

        val value  = values.head
        val fields = value.flattenWithPath
        var passed = true

        for (i <- 1 to fields.size if (passed)){
          val filter = generateQuery(fields.take(i))
          passed = checkSelectPass(filter, value)
        }

        query[JNothing.type](remove.from(collection))

        passed
      } must pass
    }

    skip("run manually")
    "Select the same value form Mock and Real Mongo for OR operator" in{
      forAll { values: List[JObject] =>
        query[JNothing.type](insert(values: _*).into(collection))

        val value1  = values.head
        val value2  = values.tail.head
        val fields1 = value1.flattenWithPath
        val fields2 = value2.flattenWithPath

        var passed = true

        for (i <- 1 to scala.math.min(fields1.size, 2); j <- 1 to scala.math.min(fields2.size, 2) if (passed)){
          val filter = generateQuery(fields1.take(i)) || generateQuery(fields2.take(j))
          passed = checkSelectPass(filter, value1, value2)
        }

        query[JNothing.type](remove.from(collection))

        passed
      } must pass
    }
  }

  private def checkSelectPass(filter: MongoFilter, values : JObject*): Boolean = {
    val selectQuery  = select().from(collection).where(filter)
    val selection    = query(selectQuery)
    val real         = selection._1.toSet
    val mock         = selection._2.toSet
    val pass         = real == mock
    if (!pass) {
      println("--------OBJECT--------")
      values.foreach{value =>
        println("----------------")
        println(Printer.compact(JsonAST.render(value)))
      }
      println("--------OBJECT--------")
      println("--------QUERY--------")
      println(Printer.compact(JsonAST.render(filter.filter)))
      println("--------QUERY--------")
      oneQuery(selectQuery, mockDatabase)
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

    filters match{
      case x :: Nil => x
      case _ => MongoAndFilter(filters)
    }
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

//        val value = JsonParser.parse("""{"995031":[false,-1.0,[""]],"585784":{"655939":[true,0.0,null,["",464012249,false,[8.988465674311579E307,[{"407323":"","20133":null,"600812":false,"538065":null,"424169":true},1,true],1868626500,1,1],[[1119944048]]],{"44603":null,"341200":true,"790448":null,"574840":"","549036":[-2147483648,"",["",""],568312444]}]},"693895":{"710558":-8.988465674311579E307,"651688":{},"642919":[],"736812":null}}""").asInstanceOf[JObject]
//        query[JNothing.type](insert(value).into(collection))
