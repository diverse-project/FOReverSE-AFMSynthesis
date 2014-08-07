package foreverse.afmsynthesis.generator

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import main.samples.randomAFMGeneration.RandomAttributedFMGenerationTest
import java.io.File
import scala.collection.JavaConversions._
import es.us.isa.generator.FM.attributed.AttributedCharacteristic
import es.us.isa.generator.FM.attributed.AttributedModel
import es.us.isa.FAMA.models.domain.Range
import es.us.isa.generator.FM.attributed.distribution.IntegerUniformDistributionFunction
import es.us.isa.generator.FM.FMGenerator
import es.us.isa.generator.FM.attributed.AttributedFMGenerator
import es.us.isa.FAMA.models.FAMAAttributedfeatureModel.FAMAAttributedFeatureModel
import es.us.isa.utils.FMWriter

class BettyGenerator extends FlatSpec with Matchers {

  val GENERATED_AFM_DIR = "generated_AFMs/"
  
    
  def generateModel(outputFile : File, characteristics : Option[AttributedCharacteristic] = None) = {
    
    val defaultCharacteristics = new AttributedCharacteristic()
    defaultCharacteristics.setNumberOfFeatures(20)
    defaultCharacteristics.setPercentageCTC(30)
    for (i <- 0 until 5) {
    	defaultCharacteristics.getAttrubitedModelList.add(
    	    new AttributedModel("Attribute" + i, 
    	        AttributedModel.TYPE_INTEGER, 
    	        new Range(0, 100), 
    	        new IntegerUniformDistributionFunction(0, 100)))
    }
    
    val gen = new FMGenerator()
    val generator = new AttributedFMGenerator(gen)
    val afm = generator.generateFM(characteristics.getOrElse(defaultCharacteristics)).asInstanceOf[FAMAAttributedFeatureModel]
    val writer = new FMWriter()
    writer.saveFM(afm, outputFile.getAbsolutePath())
  }
    
    
  "Betty" should "generate random attributed feature models" in {
//	  val afmGenerator = new RandomAttributedFMGenerationTest
//	  for (i <- 0 until 10) {
//	    // generateModel(outputDir, nbFeatures, percentageCTC, nbExtendedCTC, rangeStart, rangeEnd, nbAttributesPerFeature)
//		  afmGenerator.generateModel(new File(GENERATED_AFM_DIR), 5, 25, 2, 0, 3, 2)
//	  }
//  }
    val outputFile = new File(GENERATED_AFM_DIR + "attributedModel.afm")
    generateModel(outputFile)
    
  }
  
  
}