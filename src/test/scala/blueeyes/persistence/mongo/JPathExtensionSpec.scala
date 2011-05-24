package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.{JPath, JPathIndex, JPathField}

class JPathExtensionSpec extends Specification{
  "JPathExtension convert JPathIndex to JPathField" in {
    JPathExtension.toMongoField(JPath(JPathField("foo") :: JPathIndex(1) :: Nil)) mustEqual("foo.1")
  }
}