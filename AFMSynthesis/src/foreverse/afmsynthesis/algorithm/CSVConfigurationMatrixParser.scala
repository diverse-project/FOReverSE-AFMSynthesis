package foreverse.afmsynthesis.algorithm

import scala.collection.mutable.ListBuffer

import com.github.tototoshi.csv.CSVReader

class CSVConfigurationMatrixParser {

	def parse(path : String) : ConfigurationMatrix = {
		val reader = CSVReader.open(path)

		val labels = reader.readNext.getOrElse(Nil).toArray
		
		val configurations : ListBuffer[Array[String]] = ListBuffer.empty
		for (configuration <- reader) {
			configurations += configuration.toArray
		}
		reader.close

		new ConfigurationMatrix(labels, configurations.toList)
	}
}