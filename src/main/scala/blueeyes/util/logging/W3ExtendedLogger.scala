package blueeyes.util.logging

import java.io.{FileOutputStream, OutputStreamWriter, File, Writer}
import java.text.SimpleDateFormat
import java.util.concurrent.TimeUnit.SECONDS
import blueeyes.persistence.cache.{ExpirationPolicy, CacheSettings, Stage}
import blueeyes.util.RichThrowableImplicits
import java.util.{GregorianCalendar, Calendar, Date}
import blueeyes.parsers.W3ExtendedLogAST.FieldsDirective

object RollPolicies{
  sealed abstract class Policy
  case object Never extends Policy
  case object Hourly extends Policy
  case object Daily extends Policy
  case class Weekly(dayOfWeek: Int) extends Policy
}

import RollPolicies._
object W3ExtendedLogger{
  private val loggersCache = new scala.collection.mutable.HashMap[String, W3ExtendedLogger]

  def get(fileName: String, policy: Policy, fieldsDirective: FieldsDirective, writeDelaySeconds: Int): W3ExtendedLogger = {
    loggersCache.get(fileName) match {
      case Some(logger) =>
        logger
      case None =>
        val logger = new W3ExtendedLogger(fileName, policy, fieldsDirective, writeDelaySeconds)
        loggersCache.put(fileName, logger)
        logger
    }
  }
}

class W3ExtendedLogger(baseFileName: String, policy: Policy, fieldsDirective: FieldsDirective, writeDelaySeconds: Int){

  private val fileHandler = new FileHandler(baseFileName, policy, fieldsDirective)
  private val logStage    = new Stage[String, String](CacheSettings[String, String](ExpirationPolicy(None, Some(writeDelaySeconds), SECONDS), 100, write, 1), coalesce)

  def apply(logEntry: String){
    logStage.stop
    logStage += (("log", logEntry))
  }

  def close    = fileHandler.close

  def fileName: Option[String] = fileHandler.fileName

  private def coalesce(key: String, value1: String, value2: String) = value1 + "\n" + value2

  private def write(key: String, record: String) = fileHandler.publish(record)
}

class FileHandler(baseFileName: String, policy: Policy, fieldsDirective: FieldsDirective) extends RichThrowableImplicits with NameFormat with Roll{

  private val lock = new java.util.concurrent.locks.ReentrantReadWriteLock
  private var _fileName: Option[String]  = None
  private var _stream: Option[Writer]    = None
  private var _openTime: Long            = System.currentTimeMillis
  private var _nextRollTime: Long        = 0

  roll

  def fileName = _fileName

  def flush = _stream.foreach(_.flush())

  def close = {
    flush
    try {
      _stream.foreach(_.close())
      _stream = None
    } catch { case _ => () }
  }

  def publish(record: String) = {
    rollIfNecessary
    writeRecord(record)
  }

  private def writeRecord(record: String){
    _stream foreach { streamValue =>
      try {
        streamValue.write(record + "\n")
        streamValue.flush
      } catch {
        case e => System.err.println(e.fullStackTrace)
      }
    }
  }

  private def rollIfNecessary = {

    lock.readLock.lock()
    val shouldBeRolled = System.currentTimeMillis > _nextRollTime
    lock.readLock.unlock()

    if (shouldBeRolled){
      lock.writeLock.lock()
      try {
        if (System.currentTimeMillis > _nextRollTime) {
            roll
        }
      }
      finally {
        lock.writeLock.unlock()
      }
    }
  }

  private def openLog = {
    _stream = _fileName map { fileNameValue =>
      val dir = new File(fileNameValue).getParentFile
      if ((dir ne null) && !dir.exists) dir.mkdirs
      _openTime       = System.currentTimeMillis
      _nextRollTime   = nextRollTime(policy, System.currentTimeMillis)
      val initialize = !new File(fileNameValue).exists
      val stream     = new OutputStreamWriter(new FileOutputStream(fileNameValue, true), "UTF-8")
      if (initialize){
        stream.write("#Version: 1.0\n")
        stream.write("#Date: %s\n".format(new SimpleDateFormat("dd-MMM-yyyy HH:MM:SS").format(new Date())))
        stream.write(fieldsDirective.toString+ "\n")
        stream.flush
      }
      stream
    }
  }


  private def roll = {
    close

    val newFileName = timedName(baseFileName, policy, _openTime)

    _fileName foreach {fileNameValue => new File(fileNameValue).renameTo(new File(newFileName))}

    _fileName  = Some(newFileName)

    openLog
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