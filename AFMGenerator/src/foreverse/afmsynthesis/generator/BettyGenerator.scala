package foreverse.afmsynthesis.generator

import scala.collection.JavaConversions._

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import es.us.isa.FAMA.models.FAMAAttributedfeatureModel.FAMAAttributedFeatureModel
import es.us.isa.FAMA.models.domain.Range
import es.us.isa.generator.FM.FMGenerator
import es.us.isa.generator.FM.attributed.AttributedCharacteristic
import es.us.isa.generator.FM.attributed.AttributedFMGenerator
import es.us.isa.utils.FMWriter
import main.samples.randomAFMGeneration.RandomAttributedFMGenerationTest
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

class BettyGenerator extends FlatSpec with Matchers {

  val GENERATED_AFM_DIR = "generated_AFMs/"
    
    
  def generateModel(characteristics : AttributedCharacteristic) 
  : (FAMAAttributedFeatureModel, Long) = {
   
   val gen = new FMGenerator()
   val generator = new AttributedFMGenerator(gen)
   val afm = generator.generateFM(characteristics).asInstanceOf[FAMAAttributedFeatureModel]
   (afm, characteristics.getSeed())
  }
  
  def generateModel(
      nbFeatures : Int, 
      percentageCTC : Int, 
      nbExtendedCTC : Int, 
      rangeStart : Int, 
      rangeEnd : Int, 
      nbAttributesPerFeature : Int) 
  : (FAMAAttributedFeatureModel, Long) = {
    
	  val characteristics = new AttributedCharacteristic()
	   characteristics.setNumberOfFeatures(nbFeatures)
	   characteristics.setPercentageCTC(percentageCTC)
	   characteristics.setNumberOfExtendedCTC(nbExtendedCTC)
	   characteristics.setAttributeType(AttributedCharacteristic.INTEGER_TYPE)
	   characteristics.setDefaultValueDistributionFunction((AttributedCharacteristic.UNIFORM_DISTRIBUTION))
	   characteristics.addRange(new Range(rangeStart, rangeEnd))
	   characteristics.setNumberOfAttibutesPerFeature(nbAttributesPerFeature)
	   val argumentsDistributionFunction = Array(String.valueOf(rangeStart), String.valueOf(rangeEnd))
	   characteristics.setDistributionFunctionArguments(argumentsDistributionFunction)
	   characteristics.setHeadAttributeName("Attribute")
	   
	   generateModel(characteristics)
  }
  
   
  "Betty" should "generate random attributed feature models" in {
	  val afmGenerator = new RandomAttributedFMGenerationTest
	  for (i <- 0 until 10) {
	    val generation : Future[(FAMAAttributedFeatureModel, Long)] = future {
		  generateModel(10, 20, 5, 0, 3, 1)
	  	}
		
	    val result = try {
	    	Some(Await.result(generation, 10.seconds))
	    } catch {
	      case e : TimeoutException => None
	    }
	    
	    if (result.isDefined) {
	      val (afm, seed) = result.get
	      val writer = new FMWriter()
	      val afmFile = GENERATED_AFM_DIR + "BeTTy_" + seed + ".afm"
	      writer.saveFM(afm, afmFile)
	    }
		
	  }
	  
	  
  }
  
  

  
}