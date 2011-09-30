package blueeyes.core.service

import org.specs.Specification
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import org.mockito.Mockito.{times}
import org.mockito.Mockito
import org.specs.mock.MocksCreation

class HttpServiceBuilderSpec extends Specification with MocksCreation{
  "HttpServiceBuilder startup: creates StartupDescriptor with specified startup function" in{
    var executed = false
    val builder  = new HttpServiceBuilder[Unit]{
      val descriptor = startup(Future.sync(executed = true))
    }

    builder.descriptor.startup()

    executed must be (true)
  }
  "HttpServiceBuilder startup: creates StartupDescriptor with specified request function" in{
    val function = mock[Function[Unit, AsyncHttpService[Unit]]]
    val builder  = new HttpServiceBuilder[Unit]{
      val descriptor = request(function)
    }

    builder.descriptor.request()

    Mockito.verify(builder.descriptor.request, times(1)).apply(())
  }
  "HttpServiceBuilder shutdown: creates StartupDescriptor with specified shutdown function" in{
    val function = mock[Function[Unit, Future[Unit]]]
    val builder  = new HttpServiceBuilder[Unit]{
      val descriptor = shutdown(function)
    }

    builder.descriptor.shutdown()

    Mockito.verify(builder.descriptor.shutdown, times(1)).apply(())
  }
}
