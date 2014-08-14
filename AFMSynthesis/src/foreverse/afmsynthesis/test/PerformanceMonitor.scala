package foreverse.afmsynthesis.test

import scala.collection.mutable.ListBuffer

trait PerformanceMonitor {
  
  private val startTimes = ListBuffer.empty[(String, Long)]
  private val stopTimes = ListBuffer.empty[(String, Long)]
  private var tagStack = List.empty[String]

  def start(tag : String) {
    startTimes += tag -> System.currentTimeMillis()
    tagStack = tag :: tagStack
  }
  
  def stop(tag : String) {
    stopTimes += tag -> System.currentTimeMillis()
    tagStack = tagStack.filter(_ != tag)
  }
  
  def stopLast() {
    stopTimes += tagStack.head -> System.currentTimeMillis()
    tagStack = tagStack.tail
  }
  
  def getTimes() : List[(String, Long)] = {
    val times = ListBuffer.empty[(String, Long)]
    for ((tag, startTime) <- startTimes) {
      val stopTime = stopTimes.find(_._1 == tag)
      if (stopTime.isDefined) {
    	  val time = stopTime.get._2 - startTime
    	  times += tag -> time
      }
    }
    times.toList
  }
  
  def reset() {
    startTimes.clear
    stopTimes.clear
    tagStack = Nil
  }
  
}