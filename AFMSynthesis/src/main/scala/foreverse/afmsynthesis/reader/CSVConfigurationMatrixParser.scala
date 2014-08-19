package foreverse.afmsynthesis.reader

import scala.collection.mutable.ListBuffer
import com.github.tototoshi.csv.CSVReader
import foreverse.afmsynthesis.algorithm.ConfigurationMatrix
import scala.Array.canBuildFrom

class CSVConfigurationMatrixParser {

	def parse(path : String, dummyRoot : Boolean = true, dummyRootName : String = "root", quiet : Boolean = false) : ConfigurationMatrix = {
		val reader = CSVReader.open(path)

		var labels = reader.readNext.getOrElse(Nil)
		if (dummyRoot) {
		  labels = dummyRootName :: labels 
		}
		
		val configurations : ListBuffer[Array[String]] = ListBuffer.empty
		var nbConfigurations = 0
		val start = System.currentTimeMillis()
		
		for (configuration <- reader) {
		  
		  if (dummyRoot) {
		    configurations += "1" +: configuration.toArray
		  } else {
		    configurations += configuration.toArray
		  }
		  
		  
		  if (!quiet && nbConfigurations > 0 && nbConfigurations%10000 == 0) {
			  println(nbConfigurations + " configurations loaded")
		  } 
		  
		  nbConfigurations += 1
		}
		
		val stop = System.currentTimeMillis()
		reader.close
		
		if (!quiet) {
		  println(nbConfigurations + " configurations loaded in " + (stop - start) + "ms") 
		}
		
		
		new ConfigurationMatrix(labels.toArray, configurations.toList)
	}
}