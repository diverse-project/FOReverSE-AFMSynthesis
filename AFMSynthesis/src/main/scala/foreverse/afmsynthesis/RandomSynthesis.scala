package foreverse.afmsynthesis

import foreverse.afmsynthesis.test.AFMSynthesisTest
import java.io.File
import scala.util.Random
import foreverse.afmsynthesis.algorithm.AFMSynthesizer
import foreverse.afmsynthesis.reader.FastCSVConfigurationMatrixParser
import foreverse.afmsynthesis.writer.ModelBasedFAMAWriter
import foreverse.afmsynthesis.test.RandomMatrixGenerator
import foreverse.afmsynthesis.writer.CSVConfigurationMatrixWriter
import java.nio.file.Files
import foreverse.afmsynthesis.afm.SimpleDomainKnowledge
import com.github.tototoshi.csv.CSVWriter
import java.io.FileWriter

/**
 * @param output directory
 * @param number of variables to generate
 * @param number of configurations to generate
 * @param maximum domain size
 * @param enable or groups
 */
object RandomSynthesis extends App {
  
  println("Starting experiment...")
  
  // Parse parameters
  val dir = new File(args(0))
  val nbVariables = args(1).toInt
  val nbConfigurations = args(2).toInt
  val maximumDomainSize = args(3).toInt
  val enableOrGroups = args(4).toBoolean
  val timeoutOrGroups = if (enableOrGroups) {
    Some(args(5).toInt)
  } else {
    None
  }

  // Create output directory for the results
  val random = new Random
  val matrixName = "Random_" + nbVariables + "_" + nbConfigurations + "_" + maximumDomainSize + "_" + enableOrGroups + "_" + random.nextInt
  val outputDirPath = dir.getAbsolutePath() + "/" + matrixName + "/"
  val outputDir = new File(outputDirPath)
  
  println("Writing results in " + outputDirPath)
  outputDir.mkdirs()
  val logWriter = new FileWriter(outputDirPath + "log.txt")
  
  // Generate random input matrix
  val matrix = RandomMatrixGenerator.generateMatrix(nbVariables, nbConfigurations, maximumDomainSize)
  val matrixWriter = new CSVConfigurationMatrixWriter
  matrixWriter.writeToCSV(matrix, new File(outputDirPath + "input_matrix.csv"))
  
  // Synthesize AFM
  val synthesizer = new AFMSynthesizer
  synthesizer.perfLogger = x => logWriter.write(x.toString + "\n")
  synthesizer.synthesisLogger = x => logWriter.write(x.toString + "\n")
  val knowledge = new SimpleDomainKnowledge
  try {
	  val afm = synthesizer.synthesize(matrix, knowledge, enableOrGroups, timeoutOrGroups, outputDirPath)
	  
	  // Write results
	  val afmWriter = new ModelBasedFAMAWriter	
	  afmWriter.write(afm, new File(outputDirPath + "synthesized_afm.afm"))
	  
	  val resultWriter = new CSVWriter(new FileWriter(outputDirPath + "metrics.csv"))
	  val metrics = synthesizer.metrics
	  val times = synthesizer.getTimes
	  
	  val labels = metrics.map(_._1).toList ::: times.map(_._1)
	  resultWriter.writeRow(labels)
	  val results = metrics.map(_._2).toList ::: times.map(_._3)
	  resultWriter.writeRow(results)

	  logWriter.write("success")
	  println("success")
  } catch {
    case e : Throwable => 
      logWriter.write(e.toString())
      logWriter.write("failed")
      println("failed") 
  } finally {
	 logWriter.close()
  }
  
}