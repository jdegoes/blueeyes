package blueeyes.persistence.mongo

import org.specs.{ScalaCheck, Specification}

class MongoStageSpec extends Specification with ScalaCheck with MongoImplicits{
  private val mockMongo     = new MockMongo()
  private val mockDatabase  = mockMongo.database( "mydb" )

  "MongoStage" should{
    "store all updates" in{

    }
  }
}

class fii{

}