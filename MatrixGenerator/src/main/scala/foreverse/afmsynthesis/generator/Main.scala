package main.scala.foreverse.afmsynthesis.generator

import java.io.File

object Main {

  def main(args: Array[String]): Unit = {
    val generator = new FAMAGenerator
    generator.generateProductsFromDir(new File(args(0)))
  }

}