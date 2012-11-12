package blueeyes.logging

import akka.dispatch.Future
import akka.util.Timeout

import blueeyes.health.HealthMonitor
import blueeyes.persistence.cache.{ExpirationPolicy, Stage}
import blueeyes.util.RichThrowableImplicits

import java.io.{FileOutputStream, OutputStreamWriter, File, Writer}
import java.text.SimpleDateFormat
import java.util.concurrent.TimeUnit.SECONDS
import java.util.{GregorianCalendar, Calendar, Date}

import scalaz.Semigroup

object RollPolicies {
  sealed abstract class Policy
  case object Never extends Policy
  case object Hourly extends Policy
  case object Daily extends Policy
  case class Weekly(dayOfWeek: Int) extends Policy
}

import RollPolicies._
object RequestLogger{
  private val loggersCache = new scala.collection.mutable.HashMap[String, RequestLogger]

  def get(fileName: String, policy: Policy, fileHeader: () => String, writeDelaySeconds: Int): RequestLogger = {
    loggersCache.get(fileName) match {
      case Some(logger) =>
        logger
      case None =>
        val logger = new RequestLogger(fileName, policy, fileHeader, writeDelaySeconds)
        loggersCache.put(fileName, logger)
        logger
    }
  }
}

class RequestLogger(baseFileName: String, policy: Policy, fileHeader: () => String, writeDelaySeconds: Int, healthMonitor: HealthMonitor = HealthMonitor.Noop){
  private val fileHandler = new FileHandler(baseFileName, policy, fileHeader)
  private val logStage    = Stage[String, StringBuilder](ExpirationPolicy(None, Some(writeDelaySeconds), SECONDS), 2000) {
    (_: String, record: StringBuilder) => fileHandler.publish(record)
  }

  implicit val LogLineSemigroup = new Semigroup[StringBuilder] {
    def append(l1: StringBuilder, l2: => StringBuilder): StringBuilder = l1.append(l2)
  }

  def apply(logEntry: String) { logStage += ("log", new StringBuilder(logEntry).append("\n")) }

  def close(timeout: Timeout): Future[Unit] = logStage.flushAll(timeout).map(_ => fileHandler.close())

  def fileName: Option[String] = fileHandler.fileName
}

class FileHandler(baseFileName: String, policy: Policy, fileHeader: () => String) extends RichThrowableImplicits with NameFormat with Roll{

  private val lock = new java.util.concurrent.locks.ReentrantReadWriteLock
  private var _fileName: Option[String]  = None
  private var _stream: Option[Writer]    = None
  private var _openTime: Long            = System.currentTimeMillis
  private var _nextRollTime: Long        = 0

  roll()

  def fileName = _fileName

  def flush() {_stream.foreach(_.flush())}

  def close() {
    flush()
    try {
      _stream.foreach(_.close())
      _stream = None
    } catch { case _ => () }
  }

  def publish(record: StringBuilder) {
    rollIfNecessary()
    writeRecord(record)
  }

  private def writeRecord(record: StringBuilder){
    _stream foreach { streamValue =>
      try {
        streamValue.write(record.toArray)
        streamValue.flush()
      } catch {
        case e => System.err.println(e.fullStackTrace)
      }
    }
  }

  private def rollIfNecessary() {
    lock.readLock.lock()
    val shouldBeRolled = System.currentTimeMillis > _nextRollTime
    lock.readLock.unlock()

    if (shouldBeRolled){
      lock.writeLock.lock()
      try {
        if (System.currentTimeMillis > _nextRollTime) {
          roll()
        }
      }
      finally {
        lock.writeLock.unlock()
      }
    }
  }

  private def openLog() {
    _stream = _fileName map { fileNameValue =>
      val dir = new File(fileNameValue).getParentFile
      if ((dir ne null) && !dir.exists) dir.mkdirs
      _openTime       = System.currentTimeMillis
      _nextRollTime   = nextRollTime(policy, System.currentTimeMillis)
      val initialize = !new File(fileNameValue).exists
      val stream     = new OutputStreamWriter(new FileOutputStream(fileNameValue, true), "UTF-8")
      if (initialize){
        stream.write(fileHeader() + "\n")
        stream.flush()
      }
      stream
    }
  }


  private def roll() {
    close()

    val newFileName = timedName(baseFileName, policy, _openTime)

    _fileName foreach {fileNameValue => new File(fileNameValue).renameTo(new File(newFileName))}

    _fileName  = Some(newFileName)

    openLog()
  }
}

trait Roll{
  def nextRollTime(policy: Policy, now: Long): Long = {
    val next = new GregorianCalendar()
    next.setTimeInMillis(now)
    next.set(Calendar.MILLISECOND, 0)
    next.set(Calendar.SECOND, 0)
    next.set(Calendar.MINUTE, 0)
    policy match {
      case Never =>
        next.add(Calendar.YEAR, 100)
      case Hourly =>
        next.add(Calendar.HOUR_OF_DAY, 1)
      case Daily =>
        next.set(Calendar.HOUR_OF_DAY, 0)
        next.add(Calendar.DAY_OF_MONTH, 1)
      case Weekly(weekday) =>
        next.set(Calendar.HOUR_OF_DAY, 0)
        do {
          next.add(Calendar.DAY_OF_MONTH, 1)
        } while (next.get(Calendar.DAY_OF_WEEK) != weekday)
    }
    next.getTimeInMillis
  }

}

trait NameFormat{
  def timedName(baseFileName: String, policy: Policy, date: Long) = {
    val n = baseFileName.lastIndexOf('.')
    if (n > 0) baseFileName.substring(0, n) + "-" + timeSuffix(policy, date) + baseFileName.substring(n)
    else baseFileName + "-" + timeSuffix(policy, date)
  }

  private def timeSuffix(policy: Policy, date: Long) = {
    val dateFormat = policy match {
      case Never     => new SimpleDateFormat("yyyy")
      case Hourly    => new SimpleDateFormat("yyyyMMdd-HH")
      case Daily     => new SimpleDateFormat("yyyyMMdd")
      case Weekly(_) => new SimpleDateFormat("yyyyMMdd")
    }
    dateFormat.format(new Date(date))
  }
}
