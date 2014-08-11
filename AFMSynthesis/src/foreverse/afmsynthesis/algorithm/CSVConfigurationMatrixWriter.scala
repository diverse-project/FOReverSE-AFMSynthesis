package foreverse.afmsynthesis.algorithm

import java.io.File
import com.github.tototoshi.csv.CSVWriter
import java.io.FileWriter

class CSVConfigurationMatrixWriter {

  def writeToCSV(matrix : ConfigurationMatrix, outputFile : File) {
    val writer = new CSVWriter( new FileWriter(outputFile))
    
    writer.writeRow(matrix.labels)
    matrix.configurations.foreach(c => writer.writeRow(c))    
    writer.close
  }
  
}