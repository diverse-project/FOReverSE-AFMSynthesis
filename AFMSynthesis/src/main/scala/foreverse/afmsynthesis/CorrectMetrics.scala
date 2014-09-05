package foreverse.afmsynthesis

import java.io.File
import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.CSVWriter
import foreverse.afmsynthesis.reader.FastCSVConfigurationMatrixParser
import scala.Array.canBuildFrom

object CorrectMetrics extends App {

  val dir = new File(args(0))
  val inputMatrixPath = dir.getAbsolutePath() + "/input_matrix.csv" 
  val metricsPath = dir.getAbsolutePath() + "/metrics.csv"
  
  val parser = new FastCSVConfigurationMatrixParser
  val matrix = parser.parse(inputMatrixPath, false, "", quiet=true)
  
  val nbDistinctConfigurations = matrix.configurations.distinct.size
  val realMaximumDomainSize = matrix.labels.zipWithIndex.map(li => matrix.configurations.map(c => c(li._2)).distinct.size).max
  
  // Append results to metrics.csv file
  val csvReader = CSVReader.open(metricsPath)
  val headers = csvReader.readNext.get ::: List("#distinct configurations", "real max domain size")
  val values = csvReader.readNext.get ::: List(nbDistinctConfigurations, realMaximumDomainSize)
  csvReader.close
  
  
  val csvWriter = CSVWriter.open(metricsPath)
  csvWriter.writeRow(headers)
  csvWriter.writeRow(values)
  csvWriter.close
	  
}