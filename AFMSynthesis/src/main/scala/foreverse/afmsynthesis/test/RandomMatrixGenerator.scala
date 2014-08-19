package foreverse.afmsynthesis.test

import scala.util.Random
import foreverse.afmsynthesis.algorithm.ConfigurationMatrix
import foreverse.afmsynthesis.algorithm.ConfigurationMatrix
import scala.collection.mutable.ListBuffer

object RandomMatrixGenerator {

  def generateMatrix(nbVariables : Int, nbConfigurations : Int, maximumDomainSize : Int, seed : Option[Long] = None) 
  : ConfigurationMatrix = {
    val random = if (seed.isDefined) {
      new Random(seed.get) 
    } else {
      new Random()
    }
    
    val labels = (for (variable <- 0 until nbVariables) yield {"V" + variable}).toArray
    
    val isBoolean = labels.map(label => (label -> random.nextBoolean)).toMap
    
    val configurations = for (i <- 0 until nbConfigurations) yield {
    	for (label <- labels) yield {
    	  
    	  if (isBoolean(label)) {
    		  random.nextInt(2).toString
    	  } else {
    		  random.nextInt(maximumDomainSize).toString
    	  }
    	}
    }
    
    new ConfigurationMatrix(labels, configurations.toList)
  }
}