package foreverse.afmsynthesis.generator

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import examples.RandomConfigurations
import java.io.File
import choco.kernel.solver.ContradictionException
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import fr.familiar.attributedfm.AttributedFeatureModel
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import es.us.isa.FAMA.parser.FMFParser
import fr.familiar.attributedfm.reasoning.ChocoReasoner
import choco.cp.solver.CPSolver
import java.io.FileWriter

class FAMAGenerator extends FlatSpec with Matchers {

	val GENERATED_AFM_DIR = "../AFMGenerator/generated_AFMs/"
	val SYNTHESIZED_AFM_DIR = "../AFMSynthesis/output/synthesized/"
  
 def generateProducts(inputFile: File, outputFile: File) {
   val products = ListBuffer.empty[collection.mutable.Map[String, String]]
   val features = collection.mutable.Set.empty[String]
   val types = collection.mutable.Map.empty[String, String]
   
   val famaParser = new FMFParser()
   val model = famaParser.parseModel(inputFile.getAbsolutePath)
   
   val reasoner = new ChocoReasoner()
   val solver = new CPSolver()
   
   model.transformto(reasoner)
   val prob = reasoner.getProblem
   solver.read(prob)
   solver.propagate()
   
   
   var nbSolutions = 0
   if (solver.solve() == true && solver.isFeasible) {
     do {
       val product = collection.mutable.Map.empty[String, String]
       for (i <- 0 until prob.getNbIntVars) {
         val aux = solver.getVar(prob.getIntVar(i))
         val name = aux.getName
         val isInternalVar = name.startsWith("rel-") && name.endsWith("_card")

         if (!isInternalVar) {
           features.add(name)
           product.put(name, String.valueOf(aux.getVal))
         }
       }
       products.add(product)
       nbSolutions += 1
     } while (solver.nextSolution() == true);
   }
   
   
   val writer = new FileWriter(outputFile)
   var first = true
   for (feature <- features) {
     if (first) {
       first = false
     } else {
       writer.write(",")
     }
     writer.write(feature)
   }
   writer.write("\n")
   
   for (product <- products) {
     first = true
     for (feature <- features) {
       if (first) {
         first = false
       } else {
         writer.write(",")
       }
       val value = product(feature)
       writer.write(value)
     }
     writer.write("\n")
   }
   writer.close()
 }
	  
	  
  def generateProductsFromDir(dir : File) {
	  for (inputFile <- dir.listFiles() if inputFile.getName().endsWith(".afm")) {
		  val inputName = inputFile.getName()
		  val outputName = inputName.substring(0, inputName.length - 4) + ".csv"
		  val outputFile = new File(dir.getAbsolutePath() + "/" + outputName)
		  println("Generating products for " + inputName)
		  try {
		    val generation : Future[Unit] = future {
			  generateProducts(inputFile, outputFile)
	  		}
		  	Await.result(generation, 20.seconds)
		  } catch {
		    case e : ContradictionException => {
		      println("contradiction")
		      inputFile.delete()
		    } 
		    case e : TimeoutException => {
		      println("timeout")
		      outputFile.delete()
		    } 
		  }
		  
	  }

  }
	  
  "Product generator" should "list the product of the models generated by BeTTy in CSV files" in {
	 generateProductsFromDir(new File(GENERATED_AFM_DIR))
  }
  
  it should "list the products of the synthesized AFM" in {
    generateProductsFromDir(new File(SYNTHESIZED_AFM_DIR))
  }
  
}