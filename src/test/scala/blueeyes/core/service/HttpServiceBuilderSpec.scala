package blueeyes.core.service

import org.specs2.mutable.Specification
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import org.specs2.mock._

class HttpServiceBuilderSpec extends Specification with Mockito{
  "ServiceBuilder startup: creates StartupDescriptor with specified startup function" in{
    var executed = false
    val builder  = new ServiceBuilder[Unit]{
      val descriptor = startup(Future.sync(executed = true))
    }

    builder.descriptor.startup()

    executed must be_==(true)
  }
  "ServiceBuilder startup: creates StartupDescriptor with specified request function" in{
    val function = mock[Function[Unit, AsyncHttpService[Unit]]]
    val builder  = new ServiceBuilder[Unit]{
      val descriptor = request(function)
    }

    builder.descriptor.request()

    there was one(builder.descriptor.request).apply(())
  }
  "ServiceBuilder shutdown: creates StartupDescriptor with specified shutdown function" in{
    val function = mock[Function[Unit, Future[Unit]]]
    val builder  = new ServiceBuilder[Unit]{
      val descriptor = shutdown(function)
    }

    builder.descriptor.shutdown()

    there was one(builder.descriptor.shutdown).apply(())
  }
}
