import java.io.File

import foreverse.afmsynthesis.afm.SimpleDomainKnowledge
import foreverse.afmsynthesis.algorithm.AFMSynthesizer
import foreverse.afmsynthesis.reader.{CSVConfigurationMatrixParser, FastCSVConfigurationMatrixParser}
import foreverse.afmsynthesis.writer.ModelBasedFAMAWriter
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by gbecan on 3/19/15.
 */
class BestBuyTest extends FlatSpec with Matchers {

  val enableOrGroups = true;
  val dir = new File("input/bestbuy/")
  val OUTPUT_DIR = "output/synthesized/"

  "AFM synthesizer" should "synthesize AFMs from BestBuy PCms" in { // "AFM synthesizer"

      val parser = new CSVConfigurationMatrixParser
      val synthesizer = new AFMSynthesizer
      val writer = new ModelBasedFAMAWriter // FIXME : only support integers in attributes

      if (!enableOrGroups) {
        println("Computation of OR groups is disabled")
      }

      var nbSynthesis = 0
      var totalTime : Long = 0
      println("----------------------------------");
      for (inputFile <- dir.listFiles() if inputFile.getName().endsWith(".csv")) {
        println(inputFile.getAbsolutePath())
        val matrix = parser.parse(inputFile.getAbsolutePath, true, "root")
        val knowledge = new SimpleDomainKnowledge

        val afm = synthesizer.synthesize(matrix, knowledge, enableOrGroups, Some(3), "output/synthesized/")
        val outputFile = new File(OUTPUT_DIR + inputFile.getName().replaceAll(".csv", ".afm"))
        writer.write(afm, outputFile) // FIXME : restore this line after correcting FAMAWriter

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

        nbSynthesis += 1
        totalTime += synthesizer.getTimes.find(_._1 == "Synthesis").get._3
      }

      println("Mean synthesis time = " + totalTime / nbSynthesis + " ms");
    }

}
