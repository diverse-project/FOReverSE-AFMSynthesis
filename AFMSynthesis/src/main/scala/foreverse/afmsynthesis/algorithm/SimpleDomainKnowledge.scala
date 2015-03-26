package foreverse.afmsynthesis.algorithm

import foreverse.afmsynthesis.afm._
import foreverse.ksynthesis.mst.{OptimumBranchingFinder, WeightedImplicationGraph}
import gsd.graph.{ImplicationGraph, SimpleEdge}

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import scala.util.Random

class SimpleDomainKnowledge extends DomainKnowledge {
  
  
  val random = new Random

  override def extractFeaturesAndAttributes(matrix : ConfigurationMatrix, columnDomains : Map[String, Set[String]]) 
  : (List[Feature], List[Attribute]) = {
    
	  val features : ListBuffer[Feature] = ListBuffer.empty
	  val attributes : ListBuffer[Attribute] = ListBuffer.empty
    
	  for (label <- matrix.labels) yield {
		val values = columnDomains(label)
		if (values.forall(v => v == "0" || v == "1") ||
      values.forall(v => v == "Yes" || v == "No")) {
		  features += new Feature(label)
		} else {
			
		  val inferior = (a : String, b : String) => {
		    try {
		      val intA = a.toInt
		      val intB = b.toInt
		      intA < intB
		    } catch {
		      case e : NumberFormatException => false
		    }
		  }

      val domain = if (values.forall(v => isInteger(v) || v == "N/A")) {
        new IntegerDomain(values, "0", inferior)
      } else {
        new StringDomain(values, "N/A", inferior)
      }

		  attributes += new Attribute(label, domain)
		}
	  }

	  (features.toList, attributes.toList)
  }
  
  override def selectHierarchy(big : ImplicationGraph[Feature]) : ImplicationGraph[Feature] = {
    val hierarchyFinder = new OptimumBranchingFinder[Feature]
    val wbig = new WeightedImplicationGraph[Feature](big.clone())

    // If a feature is named "root", force it to be the root of the hierarchy
    val edgesToRemove = ListBuffer.empty[SimpleEdge]
    for (feature <- wbig.vertices() if feature.name == "root") yield {
       edgesToRemove ++= wbig.outgoingEdges(feature)
    }
    wbig.removeAllEdges(edgesToRemove)
    
    // Set random weights
    for (edge <- wbig.edges()) {
      wbig.setEdgeWeight(edge, random.nextDouble)
    }

    // Find a hierarchy
    hierarchyFinder.findOptimumBranching(wbig)
  }
  
  override def placeAttribute(attribute : Attribute, legalPositions : Set[Feature]) : Feature = {
    require(!legalPositions.isEmpty, "An attribute must have at least one possible place in the hierarchy")
    legalPositions.toList(random.nextInt(legalPositions.size))
  }
  
  override def selectOneGroup(overlappingGroups : Set[Relation]) : Relation = {
    overlappingGroups.toList(random.nextInt(overlappingGroups.size))
  }
  
  override def isTrue(feature : Feature, value : String) : Boolean = {
    value == "1" || value == "Yes"
  }
  
  override def getConstraintBound(attribute : Attribute) : String = {
    val values = attribute.domain.values.toList
    values(random.nextInt(values.size))
  }

  private def isInteger(value : String): Boolean = {
    try {
      value.toInt
      true
    } catch {
      case e : NumberFormatException => false
    }
  }

}