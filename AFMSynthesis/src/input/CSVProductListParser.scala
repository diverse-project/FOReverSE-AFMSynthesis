package input

import com.github.tototoshi.csv.CSVReader

class CSVProductListParser {

	def parse(path : String) : (List[String], List[Map[String,String]]) = {
		val reader = CSVReader.open(path)

		val products = reader.allWithHeaders		  
		val features = if (!products.isEmpty) {
			products.head.keys.toList
		} else {
			Nil
		}
		reader.close
		
		(features, products)
	}
}