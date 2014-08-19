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
    
    val varNames = (for (variable <- 0 until nbVariables) yield {"V" + variable}).toArray[String]
    
    val isBoolean = varNames.map(name => (name -> random.nextBoolean)).toMap
    
    val configurations = for (i <- 0 until nbConfigurations) yield {
    	Array("1") ++ (
    	for (varName <- varNames) yield {
    	  if (isBoolean(varName)) {
    		  random.nextInt(2).toString
    	  } else {
    		  random.nextInt(maximumDomainSize).toString
    	  }
    	})
    }
    
    val labels = Array("root") ++ varNames
    new ConfigurationMatrix(labels, configurations.toList)
  }
}