package blueeyes.core.service

import org.specs2.mutable.Specification

class ReflectiveServiceListSpec extends Specification{
  "ReflectiveServiceList: finds service if it is declared as varuable" in{
    val serviceList = new ReflectiveServiceList[Unit]{
      val service = ServiceImpl
    }

    serviceList.services mustEqual(serviceList.service :: Nil)
  }
  "ReflectiveServiceList: finds service if it is declared as methods" in{
    val serviceList = new ReflectiveServiceList[Unit]{
      def service = ServiceImpl
    }

    serviceList.services mustEqual(serviceList.service :: Nil)
  }

  object ServiceImpl extends Service[Unit, Unit]{
    def ioClass = null

    def lifecycle(context: ServiceContext) = null

    def version = ServiceVersion(1, 2, "3")

    def desc = None

    def name = null
  }
}
