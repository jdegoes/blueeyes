package blueeyes.core.service

import org.specs.Specification

class HttpReflectiveServiceListSpec extends Specification{

  "HttpReflectiveServiceList: finds service if it is declared as varuable" in{
    val serviceList = new HttpReflectiveServiceList[Unit]{
      val service = ServiceImpl
    }

    serviceList.services mustEqual(serviceList.service :: Nil)
  }
  "HttpReflectiveServiceList: finds service if it is declared as methods" in{
    val serviceList = new HttpReflectiveServiceList[Unit]{
      def service = ServiceImpl
    }

    serviceList.services mustEqual(serviceList.service :: Nil)
  }

  object ServiceImpl extends Service[Unit]{
    def ioClass = null

    def descriptorFactory = null

    def version = ServiceVersion(1, 2, "3")

    def desc = None

    def name = null
  }
}
