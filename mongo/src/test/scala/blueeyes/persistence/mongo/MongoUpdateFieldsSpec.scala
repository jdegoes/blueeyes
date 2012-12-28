package blueeyes.persistence.mongo

import org.specs2.mutable.Specification

import dsl._
import MongoUpdateOperators._
import MongoFilterOperators._
import blueeyes.json._

import scalaz._
import Scalaz._

class MongoUpdateFieldsSpec extends Specification{

  "MongoUpdateFields" should{
    "build valid json with several MongoUpdateField" in{
      (("x" inc (1)) |+| ("y" set (1))).toJValue mustEqual (JObject(JField("$inc", JObject(JField("x", JNum(1)) :: Nil)) :: JField("$set", JObject(JField("y", JNum(1)) :: Nil)) :: Nil))
    }

    "build valid json with 3 MongoUpdateFieldValues" in {
      (("x" inc (1)) |+| ("y" set (1)) |+| ("z" inc (1))).toJValue mustEqual (JObject(JField("$inc", JObject(JField("x", JNum(1)) :: JField("z", JNum(1)):: Nil)) :: JField("$set", JObject(JField("y", JNum(1)) :: Nil)) :: Nil))
    }

    "build valid json with 2 & 2 MongoUpdateFieldsValuess" in {
      (("x" inc (1)) |+| ("y" set (1)) |+| (("z" inc (1))) |+| ("b" inc(1))).toJValue mustEqual (JObject(JField("$inc", JObject(JField("x", JNum(1)) :: JField("z", JNum(1)) :: JField("b", JNum(1)) :: Nil)) :: JField("$set", JObject(JField("y", JNum(1)) :: Nil)) :: Nil))
    }
  }
}

