package foreverse.afmsynthesis.test

import scala.collection.mutable.ListBuffer

trait PerformanceMonitor {
  
  private val startTimes = ListBuffer.empty[(String, Int, Long)]
  private val stopTimes = collection.mutable.Map.empty[String, Long]
  private var tagStack = List.empty[String]
  var perfLogger : Any => Unit = println

  def top(tag : String) {
    val startTime = System.currentTimeMillis()
    val depth = tagStack.size
    startTimes += ((tag, depth, startTime))
    tagStack = tag :: tagStack
    perfLogger("  " * depth + tag)
  }
  
  def top() {
    val tag = tagStack.head
    stopTimes += tag -> System.currentTimeMillis()
    tagStack = tagStack.tail
  }
  
  def getTimes() : List[(String, Int, Long)] = {
    val times = ListBuffer.empty[(String, Int, Long)]
    for ((tag, depth, startTime) <- startTimes) {
      val stopTime = stopTimes.get(tag)
      if (stopTime.isDefined) {
    	  val time = stopTime.get - startTime
    	  times += ((tag, depth, time))
      }
    }
    times.toList
  }
  
  def resetTops() {
    startTimes.clear
    stopTimes.clear
    tagStack = Nil
  }
  
  def getMemoryUsage() : Long = {
    val runtime = Runtime.getRuntime
    runtime.gc()
    val memory = runtime.totalMemory() - runtime.freeMemory()
    val usedMB = memory
    usedMB
  }
  
}