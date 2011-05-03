package blueeyes.persistence

import blueeyes.persistence.mongo._

import scalaz.Monoid

package object mongo extends MongoImplicits{
  type MockMongo = mock.MockMongo

  implicit val MongoUpdateMonoid = new Monoid[MongoUpdate] {
    val zero = MongoUpdateNothing

    def append(u1: MongoUpdate, u2: => MongoUpdate): MongoUpdate = u1 & u2
  }
}