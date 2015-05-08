package foreverse.afmsynthesis

import java.io.{FileWriter, File}

import com.github.tototoshi.csv.CSVWriter
import foreverse.afmsynthesis.RandomSynthesis._
import foreverse.afmsynthesis.algorithm.{SimpleDomainKnowledge, AFMSynthesizer}
import foreverse.afmsynthesis.reader.CSVConfigurationMatrixParser
import foreverse.afmsynthesis.test.RandomMatrixGenerator
import foreverse.afmsynthesis.writer.{ModelBasedFAMAWriter, CSVConfigurationMatrixWriter}

import scala.util.Random

/**
 * Created by gbecan on 4/7/15.
 */
object AFMSynthesis extends App {
  println("Starting synthesis...")

  println(args.length)

  // Parse parameters
  val matrixFile = new File(args(0))
  val outputDir = new File(args(1))
  val enableOrGroups = args(2).toBoolean
  val timeoutOrGroups = if (enableOrGroups) {
    Some(args(3).toInt)
  } else {
    None
  }

  val arbitraryRoot = args(4).toBoolean
  val rootName = if (arbitraryRoot) {
    Some(args(5))
  } else {
    None
  }


  // Create output directory for the results
  val outputDirPath = outputDir.getAbsolutePath() + File.separator

  println("Writing results in " + outputDirPath)
  outputDir.mkdirs()

  // Synthesize AFM
  println("Synthesizing the AFM")

  val csvReader = new CSVConfigurationMatrixParser
  val matrix = csvReader.parse(matrixFile.getAbsolutePath, arbitraryRoot, rootName.getOrElse("root"))

  val nbVariables = matrix.labels.size
  val nbConfigurations = matrix.configurations.size
  val maximumDomainSize = matrix.labels.zipWithIndex.map(li => matrix.configurations.map(c => c(li._2)).distinct.size).max

  val synthesizer = new AFMSynthesizer

  val logWriter = new FileWriter(outputDirPath + "log.txt")
  synthesizer.perfLogger = x => logWriter.write(x.toString + "\n")
  synthesizer.synthesisLogger = x => logWriter.write(x.toString + "\n")

  val knowledge = new SimpleDomainKnowledge
  try {
    val afm = synthesizer.synthesize(matrix, knowledge, enableOrGroups, timeoutOrGroups, outputDirPath)

    synthesizer.setMetric("#variables", nbVariables.toString)

    synthesizer.setMetric("#configurations", nbConfigurations.toString)
    val nbDistinctConfigurations = matrix.configurations.size
    synthesizer.setMetric("#distinct configurations", nbDistinctConfigurations.toString)

    synthesizer.setMetric("max domain size", maximumDomainSize.toString)
    val realMaximumDomainSize = matrix.labels.zipWithIndex.map(li => matrix.configurations.map(c => c(li._2)).distinct.size).max
    synthesizer.setMetric("real max domain size", realMaximumDomainSize.toString)

    synthesizer.setMetric("enable or groups", enableOrGroups.toString)

    // Write results
    println("Writing results")
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
      println(e)
      logWriter.write(e.toString())
      logWriter.write("failed")
      println("failed")
  } finally {
    logWriter.close()
  }
}
