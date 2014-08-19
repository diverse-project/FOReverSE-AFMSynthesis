package foreverse.afmsynthesis

import foreverse.afmsynthesis.test.AFMSynthesisTest
import java.io.File

object Main extends App {

  val synthesisTest = new AFMSynthesisTest
  synthesisTest.synthesizeAFMFromDir(new File(args(0)), false)
  
}