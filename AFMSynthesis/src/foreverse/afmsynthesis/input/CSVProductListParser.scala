package foreverse.afmsynthesis.input

import com.github.tototoshi.csv.CSVReader
import scala.collection.mutable.ListBuffer

class CSVProductListParser {

	def parse(path : String) : (List[String], List[String], List[List[String]]) = {
		val reader = CSVReader.open(path)

		val features = reader.readNext.getOrElse(Nil)
		val domains = reader.readNext.getOrElse(Nil)
		
		val products : ListBuffer[List[String]] = ListBuffer()
		for (product <- reader) {
			products += product.toList
		}
		
//		val products = reader.allWithHeaders		  
//		val features = if (!products.isEmpty) {
//			products.head.keys.toList
//		} else {
//			Nil
//		}
		reader.close
		
		(features, domains, products.toList)
	}
}