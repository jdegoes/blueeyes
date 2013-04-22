package blueeyes.core.service

import blueeyes.bkka._

import akka.dispatch.Future
import akka.dispatch.Future._

import org.specs2.mock._
import org.specs2.mutable.Specification

class HttpServiceBuilderSpec extends Specification with Mockito with TestAkkaDefaults {
  "ServiceBuilder startup: creates StartupDescriptor with specified startup function" in{
    var executed = false
    val builder  = new ServiceBuilder[Unit]{
      val descriptor = startup(Future(executed = true))
    }

    builder.descriptor.startup()

    executed must be_==(true)
  }

  "ServiceBuilder startup: creates StartupDescriptor with specified request function" in{
    val function = mock[Function[Unit, AsyncHttpService[Unit, Unit]]]
    val builder  = new ServiceBuilder[Unit]{
      val descriptor = request(function)
    }

    builder.descriptor.serviceBuilder()
    there was one(builder.descriptor.serviceBuilder).apply(())
  }

  "ServiceBuilder shutdown: creates StartupDescriptor with specified shutdown function" in {
    var shutdownCalled = false

    // We need "real" behavior here to allow the Stoppable hooks to run properly
    val builder  = new ServiceBuilder[Unit]{
      val descriptor = shutdown {
        Future(shutdownCalled = true)
      }
    }

    Stoppable.stop(builder.descriptor.shutdownBuilder(()).get)
    shutdownCalled must eventually(be_==(true))
  }
}
