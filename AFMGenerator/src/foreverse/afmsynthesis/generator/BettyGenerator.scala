package foreverse.afmsynthesis.generator

import java.io.File

import scala.collection.JavaConversions._

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import es.us.isa.FAMA.models.domain.Range
import main.samples.randomAFMGeneration.RandomAttributedFMGenerationTest

class BettyGenerator extends FlatSpec with Matchers {

  val GENERATED_AFM_DIR = "generated_AFMs/"
    
    
  "Betty" should "generate random attributed feature models" in {
	  val afmGenerator = new RandomAttributedFMGenerationTest
	  for (i <- 0 until 10) {
	    // generateModel(outputDir, nbFeatures, percentageCTC, nbExtendedCTC, rangeStart, rangeEnd, nbAttributesPerFeature)
		  afmGenerator.generateModel(new File(GENERATED_AFM_DIR), 5, 25, 2, 0, 3, 2)
	  }
  }
  
}