package foreverse.afmsynthesis.afm

import foreverse.afmsynthesis.algorithm.ConfigurationMatrix
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ListBuffer
import foreverse.afmsynthesis.algorithm.BinaryImplicationGraph

class Knowledge {

  def extractFeaturesAndAttributes(matrix : ConfigurationMatrix, domains : Map[String, Domain]) 
  : (List[Feature], List[Attribute]) = {
    
	  val features : ListBuffer[Feature] = ListBuffer.empty
	  val attributes : ListBuffer[Attribute] = ListBuffer.empty
    
	  for (label <- matrix.labels) {
		val domain = domains(label)
		if (domain.values.forall(v => v == "0" || v == "1")) {
		  features += new Feature(label)
		} else {
		  attributes += new Attribute(label, domain)
		}
	  }

	  // FIXME : implicit features !
	  
	  (features.toList, attributes.toList)

  }
  
  def selectHierarchy(big : BinaryImplicationGraph) {
    // TODO : should we return something or directly assign the hierarchy in the features?
    // TODO : select a hierarchy
  }
  
  def placeAttribute(attribute : Attribute, legalPositions : List[Feature]) : Feature = {
    legalPositions.head
  }
  
}