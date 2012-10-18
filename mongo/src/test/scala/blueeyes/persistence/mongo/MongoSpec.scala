package blueeyes.persistence.mongo

import MongoFilterOperators._

import blueeyes.json._
import blueeyes.json._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import akka.util.Timeout

import org.streum.configrity.Configuration
import org.streum.configrity.io.BlockFormat

class MongoSpec extends Specification with ArbitraryJValue with ScalaCheck with MongoImplicits{
  val testLive = (new java.io.File("/etc/default/blueeyes.conf")).exists
  val config = if (testLive) 
    Configuration.load("/etc/default/blueeyes.conf", BlockFormat) 
  else
    Configuration.parse("", BlockFormat)

  private val mockMongo     = new MockMongo()
  private val mockDatabase  = mockMongo.database( "mydb" )

  private lazy val realMongo     = RealMongo(config.detach("mongo"))
  private lazy val realDatabase  = realMongo.database( "mydb" )

  private val collection    = "test-collection"

  def size = choose(20, 100).sample.get
  def genJObjects = Gen.containerOfN[List, JObject](size, genObject)
  implicit def arbJObjects: Arbitrary[List[JObject]] = Arbitrary(genJObjects)

  override def is = args(skipAll = true) ^ super.is

  "Mongo" should{
    "Explain query" in{
      val jObject  = JObject(JField("address", JObject( JField("city", JString("A")) :: JField("street", JString("1")) ::  Nil)) :: Nil)
      val jObject1 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("2")) ::  Nil)) :: Nil)
      val jObject2 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("3")) ::  Nil)) :: Nil)
      val jObject3 = JObject(JField("address", JObject( JField("city", JString("C")) :: JField("street", JString("4")) ::  Nil)) :: Nil)

      query(insert(jObject, jObject1, jObject2, jObject3).into(collection))
      query(ensureIndex("myindex").on("address.city", "address.street").in(collection))

      val explanation = oneQuery(select().from(collection).where("address.city" === "B").hint("myindex").explain, realDatabase)

      explanation \ "cursor" must_!=(JUndefined)
      explanation \ "nscannedObjects" must_!=(JUndefined)
      explanation \ "nscanned" must_!=(JUndefined)
    }
    "Select the same value with hints" in{
      val jObject  = JObject(JField("address", JObject( JField("city", JString("A")) :: JField("street", JString("1")) ::  Nil)) :: Nil)
      val jObject1 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("2")) ::  Nil)) :: Nil)
      val jObject2 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("3")) ::  Nil)) :: Nil)
      val jObject3 = JObject(JField("address", JObject( JField("city", JString("C")) :: JField("street", JString("4")) ::  Nil)) :: Nil)

      query(insert(jObject, jObject1, jObject2, jObject3).into(collection))
      query(ensureIndex("myindex").on("address.city", "address.street").in(collection))

      var passed = checkSelectPass(select().from(collection).where("address.city" === "B").hint("myindex")) &&
        checkSelectPass(select().from(collection).where("address.city" === "B").hint(List(JPath("address.city"), JPath("address.street"))))

      passed must be_==(true)
    }
    "Select the same value form Mock and Real Mongo for And operator" in{
      check { values: List[JObject] =>
        query(insert(values: _*).into(collection))

        val value  = values.head
        val fields = value.flattenWithPath
        var passed = true

        for (i <- 1 to fields.size if (passed)){
          val filter = generateQuery(fields.take(i))
          passed = checkSelectPass(filter, value)
        }

        query(remove.from(collection))

        passed
      }
    }
    "Remove the same value form Mock and Real Mongo for existing object" in{
      check { values: List[JObject] =>
        query(remove.from(collection))
        query(insert(values: _*).into(collection))

        val value  = values.head

        val filter = generateQuery(value.flattenWithPath)
        var passed = checkSelectPass(filter, value)

        query(remove.from(collection).where(filter))

        if (passed) passed = checkSelectPass(MongoFilterAll, value)
        if (passed) passed = checkSelectPass(filter, value)

        query(remove.from(collection))

        passed
      }
    }
    "Insert the same value form Mock and Real Mongo for existing object" in{
      check { values: List[JObject] =>
        query(remove.from(collection))
        query(insert(values.tail: _*).into(collection))

        val value  = values.head

        val filter = generateQuery(value.flattenWithPath)
        query(insert(value).into(collection))

        var passed = checkSelectPass(MongoFilterAll, value)
        if (passed) passed = checkSelectPass(filter, value)

        query(remove.from(collection))

        passed
      }
    }
    "Upsert the same value form Mock and Real Mongo for existing object" in{
      check { values: List[JObject] =>
        query(remove.from(collection))
        query(insert(values: _*).into(collection))

        val value  = values.head

        val newField  = genField.sample.get
        val newObject = JObject(newField :: value.fields.tail)

        val filter = generateQuery(value.flattenWithPath)
        query(upsert(collection).set(newObject).where(filter))

        var passed = checkSelectPass(MongoFilterAll, value)
        if (passed) passed = checkSelectPass(generateQuery(newObject.flattenWithPath), value)

        query(remove.from(collection))

        passed
      }
    }
    "Select the same value form Mock and Real Mongo for And operator for every field" in{
      check { vv: List[JObject] =>
        val values = List(JParser.parse("""{"201693":false,"3959":[-3.5173409829406745E307,{"775417":{"173540":false},"844904":1},false,false],"545266":null,"682503":{"926410":[true,{"468627":1642944353},""]},"162425":{"620617":true,"667941":"","61593":false,"414660":null,"605846":false}}""").asInstanceOf[JObject])
        query(insert(values: _*).into(collection))

        val value  = values.head
        val fields = value.flattenWithPath

        val passed = fields.forall{field =>
          val filters = allFilters(field)

          var passedNow = true
          for (i <- 1 to filters.size if (passedNow)){
            val filter = MongoAndFilter(filters.take(i))
            passedNow = checkSelectPass(filter, value)
          }
          passedNow
        }
        query(remove.from(collection))

        passed
      }
    }

    "Select the same value form Mock and Real Mongo for OR operator" in{
      check { values: List[JObject] =>
        query(insert(values: _*).into(collection))

        val value1  = values.head
        val value2  = values.tail.head
        val fields1 = value1.flattenWithPath
        val fields2 = value2.flattenWithPath

        var passed = true

        for (i <- 1 to scala.math.min(fields1.size, 2); j <- 1 to scala.math.min(fields2.size, 2) if (passed)){
          val filter = generateQuery(fields1.take(i)) || generateQuery(fields2.take(j))
          passed = checkSelectPass(filter, value1, value2)
        }

        query(remove.from(collection))

        passed
      }
    }
  }

  private def checkSelectPass(filter: MongoFilter, values : JObject*): Boolean = checkSelectPass(select().from(collection).where(filter), values: _*)

  private def checkSelectPass(selectQuery: MongoSelectQuery, values : JObject*): Boolean = {
    val selection    = query(selectQuery)
    val real         = selection._1.map(_.toSet).getOrElse(Set[JObject]())
    val mock         = selection._2.toSet
    val size1 = real.size
    val size2 = real.size
//    println("SIZE=" + size1 + "; " + size2)
    val pass         = real == mock
    if (!pass) {
      println("--------OBJECT--------")
      values.foreach{value =>
        println("----------------")
        println(value.renderCompact)
      }
      println("--------OBJECT--------")
      println("-REAL-")
      real.foreach{value =>
        println("--")
        println(value.renderCompact)
      }
      println("-REAL-")
      println("-MOCK-")
      mock.foreach{value =>
        println("--")
        println(value.renderCompact)
      }
      println("-MOCK-")
      println("--------QUERY--------")
      selectQuery.filter.get.filter.renderCompact
      println("--------QUERY--------")
      oneQuery(selectQuery, mockDatabase)
    }
    pass
  }

  private def query[T <: MongoQuery](query: T)(implicit m: Manifest[T#QueryResult]): (Option[T#QueryResult], T#QueryResult) = (
    if (testLive) Some(oneQuery(query, realDatabase)) else None, 
    oneQuery(query, mockDatabase)
  )

  private def oneQuery[T <: MongoQuery](query: T, database: Database)(implicit m: Manifest[T#QueryResult]) = {
    import akka.util.DurationLong
    implicit val queryTimeout = Timeout(2000)
    akka.dispatch.Await.result(database(query), queryTimeout.duration)
  }

  private def generateQuery(values: List[(JPath, JValue)]) = {
    val filters = removeUnclear(values.foldLeft(List[MongoFilter]()) {
      (result, element) => filter(element) :: result
    })

    filters match{
      case x :: Nil => x
      case _ => MongoAndFilter(filters)
    }
  }

  private def filter(pathAndValue: (JPath, JValue)) = {
    pathAndValue._2 match{
      case e: JNum    => filterForNum(pathAndValue._1, e)
      case e: JBool   => filterForBoolean(pathAndValue._1, e)
      case e: JString => filterForString(pathAndValue._1, e)
      case e: JArray  => filterForArray(pathAndValue._1, e)
      case JNull      => filterForNull(pathAndValue._1)
      case _          => MongoFieldFilter(pathAndValue._1, $eq, pathAndValue._2)
    }
  }
  private def allFilters(pathAndValue: (JPath, JValue)): List[MongoFilter] = {
    removeUnclear(pathAndValue._2 match{
      case e: JNum    => allFiltersForNum(pathAndValue._1, e)
      case e: JBool   => allFiltersForBoolean(pathAndValue._1, e)
      case e: JString => allFiltersForString(pathAndValue._1, e)
      case e: JArray  => allFiltersForArray(pathAndValue._1, e)
      case JNull      => allFiltersForNull(pathAndValue._1)
      case _          => List(MongoFieldFilter(pathAndValue._1, $eq, pathAndValue._2))
    })
  }

  private def removeUnclear(filters: List[MongoFilter]) = filters.filterNot { 
    case filter @ MongoFieldFilter(lhs, operator, rhs) => (operator == $exists || (operator == $ne && rhs == MongoPrimitiveNull)) && lhs.nodes.last.isInstanceOf[JPathIndex]
    case x => true
  }

  def filterForNull(path: JPath)                    = Gen.oneOf[MongoFilter](allFiltersForNull(path)).sample.get
  def filterForNum(path: JPath, value: JNum)        = Gen.oneOf[MongoFilter](allFiltersForNum(path, value)).sample.get
  def filterForString(path: JPath, value: JString)  = Gen.oneOf[MongoFilter](allFiltersForString(path, value)).sample.get
  def filterForArray(path: JPath, value: JArray)    = Gen.oneOf[MongoFilter](allFiltersForArray(path, value)).sample.get
  def filterForBoolean(path: JPath, value: JBool)   = Gen.oneOf[MongoFilter](allFiltersForBoolean(path, value)).sample.get

  def allFiltersForNull(path: JPath)                    = List[MongoFilter](path.hasType[JNull.type], path === JNull)
  def allFiltersForString(path: JPath, value: JString)  = List[MongoFilter](path.hasType[JString], path.isDefined, path !== JNull, path === value, path regex value.value, path !== JString(value.value + "a"), path.anyOf(MongoPrimitiveString(value.value)))
  def allFiltersForArray(path: JPath, value: JArray)    = List[MongoFilter](path.hasType[JArray], path.isDefined, path !== JNull, path === value, path !== JArray(JString("a") :: value.elements), path.hasSize(value.elements.length), path.contains[MongoPrimitive](value.elements.map(jvalueToMongoPrimitive): _*))
  def allFiltersForNum(path: JPath, value: JNum)        = List[MongoFilter](path.hasType[JNum], path.isDefined, path !== JNull, path === value, path !== JNum(value.value - 10.02E301), path > JNum(value.value - 10.02E301), path >= JNum(value.value - 10.02E301), path < JNum(value.value + 10.02E301), path <= JNum(value.value + 10.02E301), path.anyOf(MongoPrimitiveDouble(value.value.toDouble)))
  def allFiltersForBoolean(path: JPath, value: JBool)   = List[MongoFilter](path.hasType[JBool], path.isDefined, path !== JNull, path === value, path !== JBool(!value.value), path.anyOf(MongoPrimitiveBoolean(value.value)))
}
