package blueeyes.core.service

import akka.dispatch.ExecutionContext
import akka.dispatch.Future
import akka.util.Timeout

import blueeyes.bkka._
import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.data._
import blueeyes.core.http.HttpRequest
import blueeyes.core.http.HttpResponse
import blueeyes.core.data.DefaultBijections._
import blueeyes.core.http.MimeTypes._
import blueeyes.health.IntervalHealthMonitor
import blueeyes.health.metrics.eternity
import blueeyes.json._

import DefaultBijections._

import scalaz._

trait ServerHealthMonitorService extends BlueEyesServiceBuilder {
  import HttpRequestHandlerImplicits._

  implicit def executionContext: ExecutionContext
  private implicit def F: Functor[Future] = new FutureMonad(executionContext)

  def createService = {
    val monitor = new ServerHealthMonitor
    service("serverhealth", "1.0.0") { context =>
      request {
        path("/blueeyes/server/health") {
          encode[ByteChunk, Future[HttpResponse[JValue]], Future[HttpResponse[ByteChunk]]] {
            produce(application/json) {
              get {
                (request: HttpRequest[ByteChunk]) => monitor.toJValue(context).map { jv => 
                  HttpResponse[JValue](content = Some(jv))
                } 
              }
            }
          }
        }
      }
    }
  }
}

class ServerHealthMonitor(implicit executor: ExecutionContext) extends blueeyes.json.JPathImplicits {
  private implicit def stringToJString(value: String): JString = JString(value)

  val monitor = new IntervalHealthMonitor(eternity)
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
    monitor.export(path + ".init",      JNum(bean.getInit))
    monitor.export(path + ".used",      JNum(bean.getUsed))
    monitor.export(path + ".committed", JNum(bean.getCommitted))
    monitor.export(path + ".max",       JNum(bean.getMax))
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
    monitor.export("runtime.uptime",      JNum(bean.getUptime))
    monitor.export("runtime.startTime",   JNum(bean.getStartTime))
    monitor.export("runtime.currentTime", JNum(System.currentTimeMillis))
  }

  private def exportThreads() {
    val bean = ManagementFactory.getThreadMXBean

    monitor.export("threads.count",             JNum(bean.getThreadCount))
    monitor.export("threads.peakCount",         JNum(bean.getPeakThreadCount))
    monitor.export("threads.totalStartedCount", JNum(bean.getTotalStartedThreadCount))
    monitor.export("threads.daemonCount",       JNum(bean.getDaemonThreadCount))
  }

  private def exportOperatingSystem {
    val bean = ManagementFactory.getOperatingSystemMXBean

    monitor.export("operatingSystem.name",                bean.getName)
    monitor.export("operatingSystem.arch",                bean.getArch)
    monitor.export("operatingSystem.version",             bean.getVersion)
    monitor.export("operatingSystem.availableProcessors", JNum(bean.getAvailableProcessors))
    monitor.export("operatingSystem.systemLoadAverage",   JNum(bean.getSystemLoadAverage))
  }

  def toJValue(context: ServiceContext) = {
    val server =
      JObject(
        JField("server", JObject(JField("hostName", JString(context.hostName)) :: 
        JField("port", JNum(context.port)) :: JField("sslPort", JNum(context.sslPort)) :: Nil)) :: Nil
      )
    monitor.toJValue map {server.merge(_)}
  }
}
