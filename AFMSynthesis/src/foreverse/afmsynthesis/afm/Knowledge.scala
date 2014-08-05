package foreverse.afmsynthesis.afm

import foreverse.afmsynthesis.algorithm.ConfigurationMatrix
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ListBuffer
import foreverse.afmsynthesis.algorithm.BinaryImplicationGraph
import gsd.graph.ImplicationGraph
import foreverse.ksynthesis.InteractiveFMSynthesizer
import foreverse.ksynthesis.mst.OptimumBranchingFinder
import foreverse.ksynthesis.mst.WeightedImplicationGraph

class Knowledge {

  def extractFeaturesAndAttributes(matrix : ConfigurationMatrix, columnDomains : Map[String, Set[String]]) 
  : (List[Feature], List[Attribute]) = {
    
	  val features : ListBuffer[Feature] = ListBuffer.empty
	  val attributes : ListBuffer[Attribute] = ListBuffer.empty
    
	  for (label <- matrix.labels) {
		val values = columnDomains(label)
		if (values.forall(v => v == "0" || v == "1")) {
		  features += new Feature(label)
		} else {
			
		  // FIXME : determine null value
		  val nullValue = "" 
		    
		  // FIXME : determine (partial) order
		  val inferior = (a : String, b : String) => false
		  
		  val domain = new Domain(values, nullValue, inferior)
		
		  attributes += new Attribute(label, domain)
		}
	  }

	  // FIXME : implicit features !
	  
	  (features.toList, attributes.toList)

  }
  
  def selectHierarchy(big : ImplicationGraph[Feature]) : ImplicationGraph[Feature] = {
    val hierarchyFinder = new OptimumBranchingFinder[Feature]
    val wbig = new WeightedImplicationGraph[Feature](big)
    hierarchyFinder.findOptimumBranching(wbig)
    // FIXME : should we return something or directly assign the hierarchy in the features?
  }
  
  def placeAttribute(attribute : Attribute, legalPositions : List[Feature]) : Feature = {
    require(!legalPositions.isEmpty, "An attribute must have at least one possible place in the hierarchy")
    legalPositions.head
  }
  
}