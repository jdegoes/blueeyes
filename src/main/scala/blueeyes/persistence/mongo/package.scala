package blueeyes.persistence

import blueeyes.persistence.mongo._

package object mongo extends MongoImplicits{
  type MockMongo = mock.MockMongo
}