package foreverse.afmsynthesis.test

import scala.collection.mutable.ListBuffer

trait PerformanceMonitor {
  
  private val startTimes = ListBuffer.empty[(String, Int, Long)]
  private val stopTimes = collection.mutable.Map.empty[String, Long]
  private var tagStack = List.empty[String]

  def top(tag : String) {
    val startTime = System.currentTimeMillis()
    val depth = tagStack.size
    startTimes += ((tag, depth, startTime))
    tagStack = tag :: tagStack
    val name = "  " * depth + tag
    println(name)
  }
  
  def top() {
    val tag = tagStack.head
    stopTimes += tag -> System.currentTimeMillis()
    tagStack = tagStack.tail
  }
  
  def getTimes() : List[(String, Long)] = {
    val times = ListBuffer.empty[(String, Long)]
    for ((tag, depth, startTime) <- startTimes) {
      val stopTime = stopTimes.get(tag)
      if (stopTime.isDefined) {
    	  val time = stopTime.get - startTime
    	  val name = "  " * depth + tag
    	  times += name -> time
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