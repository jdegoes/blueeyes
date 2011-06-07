package blueeyes.persistence

import scalaz.Monoid

package object mongo extends blueeyes.persistence.mongo.MongoImplicits {
  type MockMongo = mock.MockMongo

  implicit val MongoUpdateMonoid = new Monoid[MongoUpdate] {
    val zero = MongoUpdateNothing

    def append(u1: MongoUpdate, u2: => MongoUpdate): MongoUpdate = u1 & u2
  }
}
