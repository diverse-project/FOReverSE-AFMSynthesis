package foreverse.afmsynthesis.input

import com.github.tototoshi.csv.CSVReader
import scala.collection.mutable.ListBuffer
import foreverse.afmsynthesis.afm.VariationPoint
import foreverse.afmsynthesis.afm.Feature
import foreverse.afmsynthesis.afm.Attribute
import foreverse.afmsynthesis.afm.domains.BooleanDomain
import foreverse.afmsynthesis.afm.domains.IntegerDomain
import foreverse.afmsynthesis.afm.domains.RealDomain
import foreverse.afmsynthesis.afm.domains.EnumDomain
import foreverse.afmsynthesis.solver.ProductListSolver
import foreverse.afmsynthesis.afm.Feature
import foreverse.afmsynthesis.afm.Attribute
import foreverse.afmsynthesis.afm.VariationPoint

class CSVProductListParser {

	def parse(path : String) : ProductList = {
		val reader = CSVReader.open(path)

		val labels = reader.readNext.getOrElse(Nil)
		val domains = reader.readNext.getOrElse(Nil)
		
		val productsVerbatim : ListBuffer[List[String]] = ListBuffer()
		for (product <- reader) {
			productsVerbatim += product.toList
		}
		reader.close
		
		
		// Transpose matrix
		val transposedProducts= productsVerbatim.toList.transpose
		val rawFeatureByProductMatrix = (labels, domains, transposedProducts).zipped.toList
		
		// Interpret values
		val variationPoints : ListBuffer[VariationPoint] = ListBuffer.empty
		val transposedInterpretedProducts : ListBuffer[List[Any]] = ListBuffer.empty
		
		for (metadata <- rawFeatureByProductMatrix) {
			val name = metadata._1
			val domain = metadata._2
			val values = metadata._3
			val (variationPoint, interpretedValues) = domain match {
			  case "BOOLEAN" => 
			    (new Feature(name), values.map(v => 
			      v match {
			        case "0" => false
			        case "1" => true
			      }))
			  case "INT(32)" => 
			    (new Attribute(name, IntegerDomain(0)),values.map(_.toInt))
			  case "DOUBLE(64)" => 
			    (new Attribute(name, RealDomain(0)),values.map(_.toDouble))
			  case "ENUM" => 
			    (new Attribute(name, EnumDomain(values.distinct.toSet, "")),values)
			}
			
			variationPoints += variationPoint
			transposedInterpretedProducts += interpretedValues
		}
				
		val interpretedProducts = transposedInterpretedProducts.toList.transpose
		
		new ProductList(variationPoints.toList, interpretedProducts)
	}
}