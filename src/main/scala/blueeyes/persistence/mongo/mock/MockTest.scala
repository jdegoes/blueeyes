package blueeyes.persistence.mongo.mock

import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo._
import blueeyes.json.{JPath, JsonParser}
import java.util.concurrent.CountDownLatch
import blueeyes.concurrent.Future

object MockTest extends MongoImplicits{
    val short  = """{"adId":"livingSocialV3","adCode":%d,"properties":{"width":300,"height":250,"advertiserId":"all","backupImageUrl":"http://static.socialmedia.com/ads/LivingSocial/CupCake/LivingSocial_Baseline_DC.jpg","clickthroughUrl":"http://www.livingsocial.com","channelId":"livingSocialChannel","campaignId":"1","groupId":"group1","clickTag":""}}"""

  private val collection = "my-collection"
  private val sort     = MongoSort("address.street", MongoSortOrderDescending)

  val mongo     = new MockMongo()
  val database  = mongo.database( "mydb" );

  def main(args: Array[String]){
    val jObjects = List.range(0, 50000) map {i => parse(short.format(i))}

    val future = database[JNothing.type](insert(jObjects: _*).into(collection))
    awaitFuture[JNothing.type](future)

    println("START SELECT")
    Thread.sleep(20000)
    println("START REAL SELECT")

    val start   = System.currentTimeMillis
    val future2 = database(select().from(collection).where(JPath("adId") === "livingSocialV3" && JPath("properties.height") < 251 && JPath("properties.width") > 200 && JPath("properties.advertiserId") === "all" && JPath("properties.advertiserId") === "all" && JPath("adCode") < 5 ).sortBy("address.city" >>))
    awaitFuture(future2)

    println("END SELECT: " + (System.currentTimeMillis - start) / 1000)
    println("SIZE=" + future2.value.get.size)

    Thread.sleep(5000)

    exit
  }

  private  def awaitFuture[T](future: Future[T]) = {
    val countDown = new CountDownLatch(1)

    future.deliverTo(v => countDown.countDown)

    countDown.await
  }

  private def parse(value: String) = JsonParser.parse(value).asInstanceOf[JObject]
}