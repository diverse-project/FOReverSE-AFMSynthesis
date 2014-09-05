package main.scala.foreverse.afmsynthesis.generator

import java.io.File
import choco.kernel.solver.ContradictionException
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.TimeoutException
import scala.concurrent.duration.DurationInt
import scala.concurrent.future
import java.util.concurrent.TimeoutException
import java.io.FileWriter
import com.github.tototoshi.csv.CSVWriter
import com.github.tototoshi.csv.CSVReader

object ProductGenerator extends App {
  
  val dir = new File(args(0))
//  val timeout = args(1).toInt
  val max = args(1).toInt
  
  val input = new File(dir.getAbsolutePath() + "/synthesized_afm.afm")
  val output = new File(dir.getAbsolutePath() + "/output_matrix.csv") 
  val metricsPath =  new File(dir.getAbsolutePath() + "/metrics.csv")
  val generator = new FAMAGenerator

  val log = new FileWriter(new File(dir.getAbsolutePath() + "/log_products.txt"))
  
  
  if (output.exists()) {
	  println("Product list already exists for "  + input.getAbsolutePath() + " ...skipping")
  } else {
	  println("Generating products...")
	  try {
//		val nbConfigurations = generator.generateProducts(input, output, timeout)
	    val (nbConfigurations, maximumReached) = generator.generateProducts(input, output, max)
	    
	    // Append results to metrics.csv file
		val csvReader = CSVReader.open(metricsPath)
		val headers = csvReader.readNext.get ::: List("#output configurations", "product generation stopped")
		val values = csvReader.readNext.get ::: List(nbConfigurations, maximumReached)
		csvReader.close
		  
		val csvWriter = CSVWriter.open(metricsPath)
		csvWriter.writeRow(headers)
		csvWriter.writeRow(values)
		csvWriter.close
  
	    
		log.write("done\n")
	    println("...done : " + nbConfigurations + " configurations generated")
	  } catch {
	    case e : ContradictionException =>
	      log.write("contradictory\n")
	      output.delete()
	      println("...failed ! Input AFM is contradictory")
	    case e : TimeoutException => 
	      log.write("timeout\n")
	      println("...timeout")
	    case e : Throwable => 
	      e.printStackTrace()
	      log.write("failed\n")
	      println("...failed")
	      output.delete()
	  }
  }
  
  log.close()
  
  
  
}