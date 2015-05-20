import java.io.File

import com.github.tototoshi.csv.CSVWriter
import foreverse.afmsynthesis.algorithm.{SimpleDomainKnowledge, AFMSynthesizer, ConfigurationMatrix}
import foreverse.afmsynthesis.reader.CSVConfigurationMatrixParser
import foreverse.afmsynthesis.writer.{CSVConfigurationMatrixWriter, ModelBasedFAMAWriter}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by gbecan on 3/19/15.
 */
class BestBuyTest extends FlatSpec with Matchers {

  val enableOrGroups = true;
  val dir = new File("input/bestbuy/")
  val OUTPUT_DIR = "output/synthesized/"

  ignore should "synthesize AFMs from BestBuy PCms" in { // "AFM synthesizer"

      val parser = new CSVConfigurationMatrixParser
      val synthesizer = new AFMSynthesizer

      if (!enableOrGroups) {
        println("Computation of OR groups is disabled")
      }

      var nbSynthesis = 0
      var totalTime : Long = 0
      println("----------------------------------");
      for (inputFile <- dir.listFiles() if inputFile.getName().endsWith(".csv")) {

        // Parsing
        println(inputFile.getAbsolutePath())
        val matrix = parser.parse(inputFile.getAbsolutePath, true, "root")

        // Interpretation
        interpret(matrix)

        val csvWriter = new CSVConfigurationMatrixWriter
        csvWriter.writeToCSV(matrix, new File(OUTPUT_DIR + "interpreted_" + inputFile.getName()))


        // Synthesis
        val knowledge = new SimpleDomainKnowledge
        val afm = synthesizer.synthesize(matrix, knowledge, enableOrGroups, Some(3), "output/synthesized/")

        // Stats
        println()
        println("Metrics")
        for ((name, value) <- synthesizer.metrics) {
          println(name + " = " + value)
        }

        println()
        println("Performances")
        for ((tag, depth, time) <- synthesizer.getTimes) {
          println(tag + ": " + time + " ms")
        }
        println("----------------------------------");

        // Output
        val writer = new ModelBasedFAMAWriter // FIXME : only support integers in attributes
        val outputFile = new File(OUTPUT_DIR + inputFile.getName().replaceAll(".csv", ".afm"))
        writer.write(afm, outputFile)

        nbSynthesis += 1
        totalTime += synthesizer.getTimes.find(_._1 == "Synthesis").get._3
      }

      println("Mean synthesis time = " + totalTime / nbSynthesis + " ms");
    }

    def interpret(matrix : ConfigurationMatrix) {

      val labels = matrix.labels

      // Compute domain of columns
      val domains : Map[String, Set[String]] = (for ((label, index) <- labels.zipWithIndex) yield {

        val values : Set[String] = (for (configuration <- matrix.configurations) yield {
          configuration(index)
        })(collection.breakOut)

        (label -> values)
      })(collection.breakOut)

      val variableTypes = domains.map { domain =>
        val label = domain._1
        val values = domain._2

        // TODO (optional) : if domain starts with integers => extract integers

        val variableType = if (values.forall(v => Set("Yes", "No", "N/A").contains(v))) {
          "Booleans"
        } else if (values.forall(v => isInteger(v) || v == "N/A")) {
          "Integers"
        } else {
          "Strings"
        }

        label -> variableType
      }

      // if domain contains N/A & booleans => N/A = No
      // if domain contains N/A & integers => N/A = 0
      // if domain contains N/A & strings => N/A = N/A

      matrix.configurations = matrix.configurations.map { product =>
        for ((cell, index) <- product.zipWithIndex) yield {
          if (cell == "N/A") {
            val variableType = variableTypes(labels(index))
            variableType match {
              case "Booleans" => "No"
              case "Integers" => "0"
              case _ => "N/A"
            }
          } else {
            cell
          }
        }
      }
    }

  def isInteger(value : String): Boolean = {
    try {
      value.toInt
      true
    } catch {
      case e : NumberFormatException => false
    }
  }

  "SPLC experiment" should "interpret the best buy dataset" in {

    val inputDir = new File("input/bestbuy/dataset")
    val outputDir = new File("input/bestbuy/dataset-interpreted")

    outputDir.mkdirs()
    val inputFiles = inputDir.listFiles()
    val parser = new CSVConfigurationMatrixParser

    val statsCSVWriter = CSVWriter.open("stats_bb.csv")
    statsCSVWriter.writeRow(Seq("min", "mean", "avg", "max"))

    for (inputFile <- inputFiles) {
      val matrix = parser.parse(inputFile.getAbsolutePath, true, "root")

      interpret(matrix)


      val domainSizes = (for (i <- 0 until matrix.labels.size) yield {
        val column = matrix.configurations.map(a => a(i))
        column.distinct.size
      }).sorted
      val avg = domainSizes.sum / domainSizes.size
      val min = domainSizes.head
      val max = domainSizes.last
      val median = domainSizes(domainSizes.size / 2)
      statsCSVWriter.writeRow(Seq(min, median, avg, max))

      val csvWriter = new CSVConfigurationMatrixWriter
      csvWriter.writeToCSV(matrix, new File(outputDir.getAbsolutePath + "/" + inputFile.getName()))
    }

    statsCSVWriter.close()

  }
}
