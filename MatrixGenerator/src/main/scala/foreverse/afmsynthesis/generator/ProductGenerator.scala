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

object ProductGenerator extends App {
  
  val dir = new File(args(0))
  val timeout = args(1).toInt
  
  val input = new File(dir.getAbsolutePath() + "/synthesized_afm.afm")
  val output = new File(dir.getAbsolutePath() + "/output_matrix.csv") 
  val generator = new FAMAGenerator

  println("Generating products...")
  try {
    val generation : Future[Unit] = future {
	  generator.generateProducts(input, output)
    }
    
    Await.result(generation, timeout.minutes)
    
    println("...done")
  } catch {
    case e : ContradictionException => println("...failed ! Input AFM is contradictory")
    case e : TimeoutException => println("...timeout")
  }
  
  
  
  
}