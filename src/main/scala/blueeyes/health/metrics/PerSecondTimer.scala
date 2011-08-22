package blueeyes.health

import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.ConcurrentHashMap
import collection.mutable.ConcurrentMap
import collection.JavaConversions._
import blueeyes.health.PerSecondTimer


class PerSecondTimer{
  private val period = 5
  private val nextCalculation = new AtomicLong(secs + period)
  private val lastCalculation = new AtomicLong(nextCalculation.get() - period)
  private val times:   ConcurrentMap[(Long, Long), AtomicLong]   = new ConcurrentHashMap[(Long, Long), AtomicLong]

  def calculate(start: Long, end: Long){
    ConcurrentMaps.createIfAbsent[Tuple2[Long, Long], AtomicLong]((start, end), times, newAtomicLong _).addAndGet(1)

    val next = nextCalculation.get()
    val now = secs
    if (now > next && lastCalculation.compareAndSet(next - period, next)){
      val toCalculate = times.filter(dates => dates._1._2 < next)
      val sorted      = toCalculate.toList.sortWith((e1, e2) => (e1._1._1 < e2._1._1))

      val calculation = sorted.foldLeft((0l, 0l, 0l, 0l)){(result, rangeAndCount) =>
        val range = rangeAndCount._1
        val count = rangeAndCount._2.get
        if (range._1 >= result._2){
          (result._1 + (range._2 - range._1), range._1, range._2, result._4 + count)
        }
        else{
          if (range._2 > result._2){
            (result._1 + (range._2 - result._3), result._1, range._2, result._4 + count)
          }
          else (result._1, result._2, result._3, result._4 + count)
        }
      }

      nextCalculation.set(next + period)
      times -- toCalculate.keySet

      Some((calculation._1, calculation._4, calculation._4 / calculation._1))
    }
    else None
  }

  private def newAtomicLong() = new AtomicLong(0)
  private def secs = System.currentTimeMillis() / 1000
}

object PerSecondTimer {
  def main(args: Array[String]){
    val timer = new PerSecondTimer()

    timer.calculate(0, 1)
    timer.calculate(0, 1)
    timer.calculate(0, 1)

    Thread.sleep(6000)

    println(timer.calculate(0, 1))
  }
}
