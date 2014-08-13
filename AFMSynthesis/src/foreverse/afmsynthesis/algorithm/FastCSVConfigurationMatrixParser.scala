package foreverse.afmsynthesis.algorithm

import java.io.BufferedReader
import java.io.FileReader
import scala.collection.mutable.ListBuffer

class FastCSVConfigurationMatrixParser extends CSVConfigurationMatrixParser {

  override def parse(path : String, dummyRoot : Boolean = true, dummyRootName : String = "root", quiet : Boolean = false) : ConfigurationMatrix = {
    val reader = new BufferedReader(new FileReader(path))
    
    var line = reader.readLine()
    var labels = if (Option(line).isDefined) {
      processLine(line).toList
    } else {
      Nil
    }
    
	if (dummyRoot) {
	  labels = dummyRootName :: labels 
	}
	
	val configurations : ListBuffer[Array[String]] = ListBuffer.empty
	
	var nbConfigurations = 0
	val start = System.currentTimeMillis()
	
	line = reader.readLine()
	while(Option(line).isDefined) {
	  val configuration = processLine(line)
	  
	  if (dummyRoot) {
	    configurations += "1" +: configuration
	  } else {
	    configurations += configuration
	  }
	  
	  if (!quiet && nbConfigurations > 0 && nbConfigurations%10000 == 0) {
		  println(nbConfigurations + " configurations loaded")
	  }
	  
	  nbConfigurations += 1
	  line = reader.readLine()
	}
	
	val stop = System.currentTimeMillis()
	reader.close
	
	if (!quiet) {
		println(nbConfigurations + " configurations loaded in " + (stop - start) + "ms")  
	}
	
	new ConfigurationMatrix(labels.toArray, configurations.toList)
  }
  
  private def processLine(line : String) : Array[String] = {
    line.split(",").toArray
  }
}