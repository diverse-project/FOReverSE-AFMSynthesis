package foreverse.afmsynthesis.algorithm

import scala.collection.mutable.ListBuffer

import com.github.tototoshi.csv.CSVReader

class CSVConfigurationMatrixParser {

	def parse(path : String, dummyRoot : Boolean = true, dummyRootName : String = "root") : ConfigurationMatrix = {
		val reader = CSVReader.open(path)

		var labels = reader.readNext.getOrElse(Nil)
		if (dummyRoot) {
		  labels = dummyRootName :: labels 
		}
		
		val configurations : ListBuffer[Array[String]] = ListBuffer.empty
		for (configuration <- reader) {
		  if (dummyRoot) {
		    configurations += "1" +: configuration.toArray
		  } else {
		    configurations += configuration.toArray
		  }
		}
		reader.close

		new ConfigurationMatrix(labels.toArray, configurations.toList)
	}
}