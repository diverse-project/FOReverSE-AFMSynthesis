package main.scala.foreverse.afmsynthesis.generator

import java.io.File
import java.io.FileWriter
import scala.collection.JavaConversions.bufferAsJavaList
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.TimeoutException
import scala.concurrent.duration.DurationInt
import scala.concurrent.future
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import choco.cp.solver.CPSolver
import choco.kernel.solver.ContradictionException
import es.us.isa.FAMA.parser.FMFParser
import fr.familiar.attributedfm.reasoning.ChocoReasoner
import es.us.isa.FAMA.models.FAMAAttributedfeatureModel.fileformats.AttributedReader
import choco.cp.solver.search.integer.varselector.MinDomain
import com.github.tototoshi.csv.CSVWriter
import java.util.concurrent.TimeoutException

class FAMAGenerator extends FlatSpec with Matchers {

	val GENERATED_AFM_DIR = "../AFMGenerator/generated_AFMs/"
	val SYNTHESIZED_AFM_DIR = "../AFMSynthesis/output/synthesized/"
  
 def generateProducts(inputFile: File, outputFile: File, timeout : Int) : Int = {
   
   val csvWriter = CSVWriter.open(outputFile)
   var nbSolutions = 0
   
   try {
   
       // Parse input AFM
	   val famaParser = new AttributedReader()
	   val model = famaParser.parseFile(inputFile.getAbsolutePath)
	   
	   // Initialize solver
	   val reasoner = new ChocoReasoner()
	   val solver = new CPSolver()
	
	   model.transformto(reasoner)
	   val prob = reasoner.getProblem
	   solver.read(prob)
	   solver.setVarIntSelector(new MinDomain(solver));
	   
	   solver.propagate()
	   
	   // Generate every possible product
	   val features = collection.mutable.Set.empty[String]
	   var sortedFeatures = features.toList.sorted
	   var firstProduct = true
	    
	   val generation : Future[Unit] = future {
		   if (solver.solve() && solver.isFeasible) {
		     do {
		       // Convert solution to product
		       val product = collection.mutable.Map.empty[String, String]
		       for (i <- 0 until prob.getNbIntVars) {
		         val aux = solver.getVar(prob.getIntVar(i))
		         val name = aux.getName
		         val isInternalVar = name.startsWith("rel-") && name.endsWith("_card")
		
		         if (!isInternalVar) {
		           if (firstProduct) {
		        	   features.add(name)
		           }
		           product.put(name, String.valueOf(aux.getVal))
		         }
		       }
		       
		       // Write features (header)
		       if (firstProduct) {
		         firstProduct = false
		         sortedFeatures = features.toList.sorted
		         csvWriter.writeRow(sortedFeatures)
		       }
		       
		       // Write product
		       val productRow = sortedFeatures.map(f => product(f))
		       csvWriter.writeRow(productRow)
		       nbSolutions += 1
		       
		     } while (solver.nextSolution());
		   }
	   }
	   
	   Await.result(generation, timeout.minutes)
	   
   } finally {
	   csvWriter.close  
   }
   
   nbSolutions
 }
	  
	  
  def generateProductsFromDir(dir : File) {
	  for (inputFile <- dir.listFiles() if inputFile.getName().endsWith(".afm")) {
		  val inputName = inputFile.getName()
		  val outputName = inputName.substring(0, inputName.length - 4) + ".csv"
		  val outputFile = new File(dir.getAbsolutePath() + "/" + outputName)
		  println("Generating products for " + inputName)
		  try {
//		    val generation : Future[Unit] = future {
			  generateProducts(inputFile, outputFile, 10)
//	  		}
//		  	Await.result(generation, 2.minutes)
		  } catch {
		    case e : ContradictionException => {
		      println("contradiction")
		      inputFile.delete()
		    } 
		    case e : TimeoutException => {
		      println("timeout")
//		      outputFile.delete()
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