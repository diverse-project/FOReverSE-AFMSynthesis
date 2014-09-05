package foreverse.afmsynthesis

import foreverse.afmsynthesis.reader.FastCSVConfigurationMatrixParser
import java.io.File
import java.io.FileWriter
import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.CSVWriter
import java.io.FileReader
import java.io.BufferedReader

object ConfigurationSemanticsChecker extends App {

  val dir = new File(args(0))
  
  val inputMatrixPath = dir.getAbsolutePath() + "/input_matrix.csv" 
  val outputMatrixPath = dir.getAbsolutePath() + "/output_matrix.csv"
  val metricsPath = dir.getAbsolutePath() + "/metrics.csv"
  val logPath = dir.getAbsolutePath() + "/log_products.csv"
  
  val inputMatrixFile = new File(inputMatrixPath)
  val outputMatrixFile = new File(outputMatrixPath)
  val logFile = new File(logPath)
  
  if (!inputMatrixFile.exists()) {
	println("Input matrix does not exist")
  } else if (!outputMatrixFile.exists()) {
    println("Output matrix does not exist")
  } else {
    
	  println("Parsing matrices")
	  val parser = new FastCSVConfigurationMatrixParser
	  val inputMatrix = parser.parse(inputMatrixPath, false, quiet=true)
	  val outputMatrix = parser.parse(outputMatrixPath, false, quiet=true)
	  
	  val logReader = new BufferedReader(new FileReader(logFile))
	  val status = logReader.readLine() 
	  logReader.close()
	  
	  // Create a dictionary to translate column positions between input and output matrices
	  println("Creating dictionary")
	  val dictionary = collection.mutable.Map.empty[Int, Int]
		for ((inLabel, inIndex) <- inputMatrix.labels.zipWithIndex) {
			val outVar = outputMatrix.labels.zipWithIndex.find(_._1.endsWith(inLabel))
			if (outVar.isDefined) {
			  val (outLabel, outIndex) = outVar.get 
			  dictionary += inIndex -> outIndex
			}
		}

	  // Check completeness of algorithm
	  println("Checking completeness")
	  for (inConfig <- inputMatrix.configurations) {
		  val outConfig = outputMatrix.configurations.find{ outConfig =>
		  	inConfig.zipWithIndex.forall(value =>
		  		!dictionary.contains(value._2) 
		  		|| (value._1 == outConfig(dictionary(value._2)))
		  	)
		  }

		  assert(outConfig.isDefined, inConfig.mkString(",") + " does not exist in output configurations")
	  }
	  
	  // Compute overapproximation of the algorithm
	  val nbInputConfigurations = inputMatrix.configurations.distinct.size 
	  val nbOutputConfigurations = outputMatrix.configurations.size 
	  val overApproximation = ((nbOutputConfigurations - nbInputConfigurations) * 100).toDouble / nbOutputConfigurations.toDouble

	  // Append results to metrics.csv file
	  val csvReader = CSVReader.open(metricsPath)
	  val headers = csvReader.readNext.get ::: List("#output configurations", "%incorrect configurations", "product generation status")
	  val values = csvReader.readNext.get ::: List(nbOutputConfigurations, overApproximation, status)
	  csvReader.close
	  
	  
	  val csvWriter = CSVWriter.open(metricsPath)
	  csvWriter.writeRow(headers)
	  csvWriter.writeRow(values)
	  csvWriter.close
	  
	  println(overApproximation + "% of generated configurations do not exist in input matrix")
  }
  
}