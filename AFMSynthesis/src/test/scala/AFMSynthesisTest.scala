import java.io.File

import foreverse.afmsynthesis.algorithm.{SimpleDomainKnowledge, AFMSynthesizer}
import foreverse.afmsynthesis.reader.FastCSVConfigurationMatrixParser
import foreverse.afmsynthesis.test.RandomMatrixGenerator
import foreverse.afmsynthesis.writer.{CSVConfigurationMatrixWriter, ModelBasedFAMAWriter}
import org.scalatest.{FlatSpec, Matchers}

import scala.Array.canBuildFrom
import scala.util.Random

class AFMSynthesisTest extends FlatSpec with Matchers{
  
  val INPUT_DIR = "input/test-set/"
  val GENERATED_DIR = "input/generated/"
  val OUTPUT_DIR = "output/synthesized/"
    
  def synthesizeAFMFromDir(dir : File, enableOrGroups : Boolean, dummyRoot : Boolean, rootName : String => String = _ => "root") {
	  val parser = new FastCSVConfigurationMatrixParser
	  val synthesizer = new AFMSynthesizer
	  val writer = new ModelBasedFAMAWriter
	  
	  if (!enableOrGroups) {
		  println("Computation of OR groups is disabled")
	  }
	  

	  var nbSynthesis = 0
	  var totalTime : Long = 0
	  println("----------------------------------");
	  for (inputFile <- dir.listFiles() if inputFile.getName().endsWith(".csv")) {
      println(inputFile.getAbsolutePath())
      val matrix = parser.parse(inputFile.getAbsolutePath, dummyRoot, rootName(inputFile.getName()))
      val knowledge = new SimpleDomainKnowledge

      val afm = synthesizer.synthesize(matrix, knowledge, enableOrGroups, Some(3), "output/synthesized/")
      val outputFile = new File(OUTPUT_DIR + inputFile.getName().replaceAll(".csv", ".afm"))
      writer.write(afm, outputFile)

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

  ignore should "synthesize AFM from the test set" in { // "AFM synthesis algorithm"
	val dir = new File(INPUT_DIR)
	synthesizeAFMFromDir(dir, true, false, _ => "root")
  }
  
  
  ignore should "synthesize AFM from randomly generated AFMs" in {
    val dir = new File(GENERATED_DIR)
    synthesizeAFMFromDir(dir, false, false)
  }

//  it should "synthesize a specific directory" in {
//    val dir = new File("check_that_Random_1000_100_10_false_1980594031")
//    synthesizeAFMFromDir(dir, false, false)
//  }

  ignore should "be complete" in {
    val parser = new FastCSVConfigurationMatrixParser
    val inputDir = new File(INPUT_DIR)
//    val inputDir = new File(INPUT_DIR)
    
    for (inputFile <- inputDir.listFiles() if inputFile.getName().endsWith(".csv")) {
      println(inputFile.getAbsolutePath())
      val outputFile = new File(OUTPUT_DIR + inputFile.getName())
      
      if (outputFile.exists()) {
	      val inputMatrix = parser.parse(inputFile.getAbsolutePath(), false, quiet=true)
	      val outputMatrix = parser.parse(outputFile.getAbsolutePath(), false, quiet=true)
	      
	      // Create a dictionary to translate column positions between input and output matrices
	      val dictionary = collection.mutable.Map.empty[Int, Int]
	      for ((inLabel, inIndex) <- inputMatrix.labels.zipWithIndex) {
	        val outVar = outputMatrix.labels.zipWithIndex.find(_._1.endsWith(inLabel))
	        if (outVar.isDefined) {
	        	val (outLabel, outIndex) = outVar.get 
	        	dictionary += inIndex -> outIndex
	        }
	      }
	      
	      // Check completeness of algorithm
	      for (inConfig <- inputMatrix.configurations) {
	        val outConfig = outputMatrix.configurations.find{ outConfig =>
	        	inConfig.zipWithIndex.forall(value =>
	        	  !dictionary.contains(value._2) ||
	        	  (value._1 == outConfig(dictionary(value._2)))
	        	)
	        }
	        
	        assert(outConfig.isDefined, inConfig.mkString(",") + " does not exist in output configurations")
	      }
	      val nbInputConfigurations = inputMatrix.configurations.size 
	      val nbOutputConfigurations = outputMatrix.configurations.size 
	      val overApproximation = ((nbOutputConfigurations - nbInputConfigurations) * 100) / nbOutputConfigurations 
	      println(overApproximation + "% of generated configurations do not exist in input matrix")
      
      } else {
        println("no configuration matrix for " + inputFile.getAbsolutePath())
      }
      
    }
  }
  
  ignore should "generate random matrices" in { //"Random matrix generator"
    val nbMatrices = 10
    val nbVariables = 100
    val nbConfigurations = 5000
    val maximumDomainSize = 10
    
    val random = new Random
    val writer = new CSVConfigurationMatrixWriter
    
    for (i <- 0 until nbMatrices) {
    	val name = "Random_" + nbVariables + "_" + nbConfigurations + "_" + maximumDomainSize + "_" + random.nextInt + ".csv"
    	val matrix = RandomMatrixGenerator.generateMatrix(nbVariables, nbConfigurations, maximumDomainSize)
    	writer.writeToCSV(matrix, new File(GENERATED_DIR + name))
    }
    
  }
  
  
}