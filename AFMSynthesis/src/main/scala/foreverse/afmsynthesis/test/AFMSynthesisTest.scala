package foreverse.afmsynthesis.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import scala.io.Source
import foreverse.afmsynthesis.reader.CSVConfigurationMatrixParser
import foreverse.afmsynthesis.algorithm.AFMSynthesizer
import foreverse.afmsynthesis.afm.Knowledge
import com.github.tototoshi.csv.CSVReader
import java.io.BufferedReader
import java.io.FileReader
import foreverse.afmsynthesis.reader.FastCSVConfigurationMatrixParser
import foreverse.afmsynthesis.writer.FAMAWriter
import scala.util.Random
import foreverse.afmsynthesis.writer.CSVConfigurationMatrixWriter
import foreverse.afmsynthesis.reader.CSVConfigurationMatrixParser
import foreverse.afmsynthesis.writer.TextualFAMAWriter
import foreverse.afmsynthesis.writer.ModelBasedFAMAWriter

class AFMSynthesisTest extends FlatSpec with Matchers{
  
  val INPUT_DIR = "input/test-set/"
  val GENERATED_DIR = "input/generated/"
  val OUTPUT_DIR = "output/synthesized/"
    
  def synthesizeAFMFromDir(dir : File, dummyRoot : Boolean, rootName : String => String = _ => "root") {
	  val parser = new FastCSVConfigurationMatrixParser
	  val synthesizer = new AFMSynthesizer
	  val writer = new ModelBasedFAMAWriter
	  
	  println("----------------------------------");
	  for (inputFile <- dir.listFiles() if inputFile.getName().endsWith(".csv")) {
		println(inputFile.getAbsolutePath())
		val matrix = parser.parse(inputFile.getAbsolutePath, dummyRoot, rootName(inputFile.getName()))
		val knowledge = new Knowledge

		val afm = synthesizer.synthesize(matrix, knowledge)
		val outputFile = new File(OUTPUT_DIR + inputFile.getName().replaceAll(".csv", ".afm"))
		writer.write(afm, outputFile)
		
		println()
		println("Performances")
		for ((tag, time) <- synthesizer.getTimes) {
		  println(tag + ": " + time + " ms")
		}
		println("----------------------------------");
	  }

  }

  "AFM synthesis algorithm" should "synthesize AFM from the test set" in {
	val dir = new File(INPUT_DIR)
	synthesizeAFMFromDir(dir, false, _ => "root")
  }
  
  
  it should "synthesize AFM from randomly generated AFMs" in {
    val dir = new File(GENERATED_DIR)
    synthesizeAFMFromDir(dir, true)
  }
  
  it should "be complete" in {
    val parser = new FastCSVConfigurationMatrixParser
    val inputDir = new File(GENERATED_DIR)
    
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
  
  "Random matrix generator" should "generate random matrices" in {
    val nbMatrices = 10
    val nbVariables = 5
    val nbConfigurations = 100
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