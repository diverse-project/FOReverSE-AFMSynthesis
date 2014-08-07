package foreverse.afmsynthesis.generator

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import main.samples.randomAFMGeneration.RandomAttributedFMGenerationTest
import java.io.File

class BettyGenerator extends FlatSpec with Matchers {

  val GENERATED_AFM_DIR = "generated_AFMs/"
  
  "Betty" should "generate random attributed feature models" in {
	  val afmGenerator = new RandomAttributedFMGenerationTest
	  for (i <- 0 to 10) {
		  afmGenerator.generateModel(new File(GENERATED_AFM_DIR), 5, 30, 30, 0, 10, 2)
	  }


  }
  
}