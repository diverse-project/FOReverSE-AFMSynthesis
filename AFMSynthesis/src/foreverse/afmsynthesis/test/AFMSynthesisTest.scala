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

class AFMSynthesisTest extends FlatSpec with Matchers{
  
  val INPUT_DIR = "input/test-set/"
  val GENERATED_DIR = "input/generated/"
    
  def synthesizeAFMFromDir(dir : File, dummyRoot : Boolean, rootName : String => String = _ => "root") {
	  val parser = new FastCSVConfigurationMatrixParser
	  val synthesizer = new AFMSynthesizer
	  
	  println("----------------------------------");
	  for (inputFile <- dir.listFiles() if inputFile.getName().endsWith(".csv")) {
		println(inputFile.getAbsolutePath())
		val matrix = parser.parse(inputFile.getAbsolutePath, dummyRoot, rootName(inputFile.getName()))
		val knowledge = new Knowledge

		synthesizer.synthesize(matrix, knowledge)
		println("----------------------------------");
	  }

  }

  "It" should "work" in {
	val dir = new File(INPUT_DIR)
	synthesizeAFMFromDir(dir, true, _ => "root")
  }
  
  
  it should "synthesize AFM from randomly generated AFMs" in {
    val dir = new File(GENERATED_DIR)
    synthesizeAFMFromDir(dir, false)
  }
  
  
  "CSV parser" should "read a huge CSV file fast" in {
//	val reader = CSVReader.open(GENERATED_DIR + "BeTTy_803515568.csv")
	
	val reader = new BufferedReader(new FileReader(GENERATED_DIR + "BeTTy_803515568.csv"))
	val start = System.currentTimeMillis()

	var i = 0
	var line = reader.readLine()
	while(Option(line).isDefined) {
	  val config = line.split(",").toArray
//	  println(config.mkString(","))
//	  for (char <- line) {
//	    
//	  }
	  line = reader.readLine()
	  i += 1 
	}
	println(i)
//	for (i <- 0 until 100000) {
//	  reader.readNext
//	}
//	
	val stop = System.currentTimeMillis()
	
	reader.close
	
	println(stop - start);
	
  }
 
  
}