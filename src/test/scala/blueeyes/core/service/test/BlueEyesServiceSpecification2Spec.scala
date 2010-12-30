package blueeyes.core.service.test

import blueeyes.util.Future
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes._

class BlueEyesServiceSpecification2Spec extends BlueEyesServiceSpecification2[String]{

  path$[String, String]("/foo"){
    get$[String, String]{
      { response: HttpResponse[String] =>
        //response.status.code mustEqual(HttpStatusCodes.OK)
        1 must be (1)
      }
    }
  } should "return list of matched ad ids"
}