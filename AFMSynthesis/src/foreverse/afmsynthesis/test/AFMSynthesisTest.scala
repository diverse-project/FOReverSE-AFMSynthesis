package foreverse.afmsynthesis.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import scala.io.Source
import foreverse.afmsynthesis.algorithm.CSVConfigurationMatrixParser
import foreverse.afmsynthesis.algorithm.AFMSynthesizer
import foreverse.afmsynthesis.afm.Knowledge
import com.github.tototoshi.csv.CSVReader
import java.io.BufferedReader
import java.io.FileReader
import foreverse.afmsynthesis.algorithm.FastCSVConfigurationMatrixParser
import foreverse.afmsynthesis.algorithm.FAMAWriter

class AFMSynthesisTest extends FlatSpec with Matchers{
  
  val INPUT_DIR = "input/test-set/"
  val GENERATED_DIR = "input/generated/"
  val OUTPUT_DIR = "output/synthesized/"
    
  def synthesizeAFMFromDir(dir : File, dummyRoot : Boolean, rootName : String => String = _ => "root") {
	  val parser = new FastCSVConfigurationMatrixParser
	  val synthesizer = new AFMSynthesizer
	  val writer = new FAMAWriter
	  
	  println("----------------------------------");
	  for (inputFile <- dir.listFiles() if inputFile.getName().endsWith(".csv")) {
		println(inputFile.getAbsolutePath())
		val matrix = parser.parse(inputFile.getAbsolutePath, dummyRoot, rootName(inputFile.getName()))
		val knowledge = new Knowledge

		val afm = synthesizer.synthesize(matrix, knowledge)
		val outputFile = new File(OUTPUT_DIR + inputFile.getName().replaceAll(".csv", ".afm"))
		writer.write(afm, outputFile)
		println("----------------------------------");
	  }

  }

  "AFM synthesizer" should "synthesize AFM from the test set" in {
	val dir = new File(INPUT_DIR)
	synthesizeAFMFromDir(dir, true, _ => "root")
  }
  
  
  it should "synthesize AFM from randomly generated AFMs" in {
    val dir = new File(GENERATED_DIR)
    synthesizeAFMFromDir(dir, false)
  }
  
 
  
}