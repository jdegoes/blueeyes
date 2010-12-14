package blueeyes.core.service

import org.spex.Specification
import blueeyes.util.Future
import blueeyes.util.FutureImplicits._
import org.mockito.Mockito.{times}
import org.mockito.Mockito

class HttpServiceBuilderSpec extends Specification{
  "HttpServiceBuilder startup: creates StartupDescriptor with specified startup function" in{
    var executed = false
    val builder  = new HttpServiceBuilder[Unit]{
      val descriptor = startup({
        executed = true
        ()
      })
    }

    builder.descriptor.startup()

    executed must be (true)
  }
  "HttpServiceBuilder startup: creates StartupDescriptor with specified request function" in{
    val function = mock[Function[Unit, HttpRequestHandler[Unit]]]
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