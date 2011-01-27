package blueeyes.core.service

import blueeyes.json.JsonAST._
import blueeyes.core.data._
import blueeyes.{BlueEyesServiceBuilderBase, BlueEyesServiceBuilder, BlueEyesServiceBuilderString}
import blueeyes.core.http.HttpResponse

trait ServerHealthMonitorServiceBase[T] extends BlueEyesServiceBuilderBase[T] with ServerHealthMonitor {
  def createService(implicit jValueBijection: Bijection[JValue, T], m: Manifest[T]) = service("serverhealth", "1.0.0"){ context =>
    request{
      path("/blueeyes/server/health") {
        get { request =>
          HttpResponse[T](content=Some(jValueBijection(health)))
        }
      }
    }
  }
}

trait ServerHealthMonitorService extends ServerHealthMonitorServiceBase[Array[Byte]] with BlueEyesServiceBuilder{
  def serverHealthMonitorService = createService
}

trait ServerHealthMonitorServiceString extends ServerHealthMonitorServiceBase[String] with BlueEyesServiceBuilderString{
  def serverHealthMonitorService = createService
}

trait ServerHealthMonitor{
  private implicit def stringToJValue(v: String) = JString(v)
  private implicit def longToJValue(v: Long)   = JInt(v)
  private implicit def longToJValue(v: Int)    = JInt(v)

  import java.lang.management._  
  def health = {
    JObject(JField("runtime", runtime) :: JField("memory", memory) :: JField("threads", threads) :: JField("operatingSystem", operatingSystem) :: Nil)
  }

  private def memory = {
    val bean = ManagementFactory.getMemoryMXBean()

    JObject(List(JField("heap", memoryUsage(bean.getHeapMemoryUsage())), JField("nonHeap", memoryUsage(bean.getNonHeapMemoryUsage()))))
  }

  private def memoryUsage(bean: MemoryUsage) = {
    JObject(List(JField("init", bean.getInit()), JField("used", bean.getUsed()), JField("committed", bean.getCommitted()), JField("max", bean.getMax())))
  }

  private def runtime = {
    val bean = ManagementFactory.getRuntimeMXBean()

    JObject(List(JField("vmName", bean.getVmName()), JField("vmVendor", bean.getVmVendor()), JField("vmVersion", bean.getVmVersion()),
           JField("specName", bean.getSpecName()), JField("specVendor", bean.getSpecVendor()), JField("specVersion", bean.getSpecVersion()),
           JField("classPath", bean.getClassPath()), JField("libraryPath", bean.getLibraryPath()), JField("specVersion", bean.getSpecVersion()),
           JField("uptime", bean.getUptime()), JField("startTime", bean.getStartTime()), JField("currentTime", System.currentTimeMillis)
    ))
  }

  private def threads = {
    val bean = ManagementFactory.getThreadMXBean()

    JObject(List(JField("count", bean.getThreadCount()), JField("peakCount", bean.getPeakThreadCount()), JField("totalStartedCount", bean.getTotalStartedThreadCount()),
           JField("daemonCount", bean.getDaemonThreadCount())))
  }

  private def operatingSystem = {
    val bean = ManagementFactory.getOperatingSystemMXBean()

    JObject(List(JField("name", bean.getName()), JField("arch", bean.getArch()), JField("version", bean.getVersion())))
  }
}