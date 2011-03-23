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
          HttpResponse[T](content=Some(jValueBijection(toJValue(context))))
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

trait ServerHealthMonitor extends blueeyes.json.Implicits with blueeyes.json.JPathImplicits{

  private val monitor = new blueeyes.health.HealthMonitor()

  exportMemory
  exportRuntime
  exportThreads
  exportOperatingSystem

  import java.lang.management._  

  private def exportMemory = {
    val bean = ManagementFactory.getMemoryMXBean()

    exportMemoryUsage("memory.heap", bean.getHeapMemoryUsage())
    exportMemoryUsage("memory.nonHeap", bean.getNonHeapMemoryUsage())
  }

  private def exportMemoryUsage(path: String, bean: MemoryUsage) = {
    monitor.export(path + ".init",      bean.getInit)
    monitor.export(path + ".used",      bean.getUsed)
    monitor.export(path + ".committed", bean.getCommitted)
    monitor.export(path + ".max",       bean.getMax)
  }

  private def exportRuntime = {
    val bean = ManagementFactory.getRuntimeMXBean()

    monitor.export("runtime.vmName",      bean.getVmName)
    monitor.export("runtime.vmVendor",    bean.getVmVendor)
    monitor.export("runtime.vmVersion",   bean.getVmVersion)
    monitor.export("runtime.specName",    bean.getSpecName)
    monitor.export("runtime.specVendor",  bean.getSpecVendor)
    monitor.export("runtime.specVersion", bean.getSpecVersion)
    monitor.export("runtime.classPath",   bean.getClassPath)
    monitor.export("runtime.libraryPath", bean.getLibraryPath)
    monitor.export("runtime.uptime",      bean.getUptime)
    monitor.export("runtime.startTime",   bean.getStartTime)
    monitor.export("runtime.currentTime", System.currentTimeMillis)
  }

  private def exportThreads = {
    val bean = ManagementFactory.getThreadMXBean()

    monitor.export("threads.count",             bean.getThreadCount)
    monitor.export("threads.peakCount",         bean.getPeakThreadCount)
    monitor.export("threads.totalStartedCount", bean.getTotalStartedThreadCount)
    monitor.export("threads.daemonCount",       bean.getDaemonThreadCount)
  }

  private def exportOperatingSystem = {
    val bean = ManagementFactory.getOperatingSystemMXBean()

    monitor.export("operatingSystem.name",                bean.getName)
    monitor.export("operatingSystem.arch",                bean.getArch)
    monitor.export("operatingSystem.version",             bean.getVersion)
    monitor.export("operatingSystem.availableProcessors", bean.getAvailableProcessors)
    monitor.export("operatingSystem.systemLoadAverage",   bean.getSystemLoadAverage)
  }

  def toJValue(context: HttpServiceContext) = {
    val server     = JObject(JField("server", JObject(JField("hostName", JString(context.hostName)) :: JField("port", context.port) :: JField("sslPort", context.sslPort) :: Nil)) :: Nil)
    server.merge(monitor.toJValue)
  }
}