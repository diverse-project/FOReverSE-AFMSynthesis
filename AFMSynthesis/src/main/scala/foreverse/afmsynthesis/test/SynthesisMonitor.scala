package foreverse.afmsynthesis.test

import scala.collection.mutable.ListBuffer

trait SynthesisMonitor {

  val metrics = ListBuffer.empty[(String, String)]
  var synthesisLogger : Any => Unit = println
  
  def setMetric(metric : String, value : String) {
    metrics += metric -> value
  }
  
  def log(something : Any) {
    synthesisLogger(something)
  }
  
  def resetMetrics() {
    metrics.clear
  }
  
}