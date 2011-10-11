package blueeyes.core.service

import blueeyes.json.JsonAST._
import blueeyes.concurrent.Future
import blueeyes.core.data._
import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.http.HttpResponse
import blueeyes.core.data.BijectionsChunkJson
import blueeyes.core.http.MimeTypes._
import blueeyes.health.IntervalHealthMonitor
import blueeyes.health.metrics.eternity

trait ServerHealthMonitorService extends BlueEyesServiceBuilder with ServerHealthMonitor with BijectionsChunkJson{
  def createService = service("serverhealth", "1.0.0"){ context =>
    request{
      path("/blueeyes/server/health") {
        produce(application/json){
          get { request =>
            Future.async {
              HttpResponse[JValue](content=Some(toJValue(context)))
            }
          }
        }
      }
    }
  }
}

trait ServerHealthMonitor extends blueeyes.json.Implicits with blueeyes.json.JPathImplicits{

  private val monitor = new IntervalHealthMonitor(eternity)
  exportMemory
  exportRuntime
  exportThreads
  exportOperatingSystem

  import java.lang.management._  

  private def exportMemory() {
    val bean = ManagementFactory.getMemoryMXBean

    exportMemoryUsage("memory.heap", bean.getHeapMemoryUsage)
    exportMemoryUsage("memory.nonHeap", bean.getNonHeapMemoryUsage)
  }

  private def exportMemoryUsage(path: String, bean: MemoryUsage) {
    monitor.export(path + ".init",      bean.getInit)
    monitor.export(path + ".used",      bean.getUsed)
    monitor.export(path + ".committed", bean.getCommitted)
    monitor.export(path + ".max",       bean.getMax)
  }

  private def exportRuntime() {
    val bean = ManagementFactory.getRuntimeMXBean

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

  private def exportThreads() {
    val bean = ManagementFactory.getThreadMXBean

    monitor.export("threads.count",             bean.getThreadCount)
    monitor.export("threads.peakCount",         bean.getPeakThreadCount)
    monitor.export("threads.totalStartedCount", bean.getTotalStartedThreadCount)
    monitor.export("threads.daemonCount",       bean.getDaemonThreadCount)
  }

  private def exportOperatingSystem {
    val bean = ManagementFactory.getOperatingSystemMXBean

    monitor.export("operatingSystem.name",                bean.getName)
    monitor.export("operatingSystem.arch",                bean.getArch)
    monitor.export("operatingSystem.version",             bean.getVersion)
    monitor.export("operatingSystem.availableProcessors", bean.getAvailableProcessors)
    monitor.export("operatingSystem.systemLoadAverage",   bean.getSystemLoadAverage)
  }

  def toJValue(context: ServiceContext) = {
    val server     = JObject(JField("server", JObject(JField("hostName", JString(context.hostName)) :: JField("port", context.port) :: JField("sslPort", context.sslPort) :: Nil)) :: Nil)
    server.merge(monitor.toJValue)
  }
}
