package foreverse.afmsynthesis.afm

import foreverse.afmsynthesis.algorithm.ConfigurationMatrix
import scala.collection.immutable.Map
import gsd.graph.ImplicationGraph
import scala.collection.mutable.ListBuffer
import foreverse.ksynthesis.mst.OptimumBranchingFinder
import foreverse.ksynthesis.mst.WeightedImplicationGraph
import scala.collection.JavaConversions._

class AFMDomainKnowledge(val afm : AttributedFeatureModel) extends DomainKnowledge {
  
  
  override def extractFeaturesAndAttributes(matrix : ConfigurationMatrix, columnDomains : Map[String, Set[String]]) : (List[Feature], List[Attribute]) = {
    val kFeatures = afm.diagram.features
    val kAttributes = kFeatures.flatMap(_.attributes)
    
    val features : ListBuffer[Feature] = ListBuffer.empty
	val attributes : ListBuffer[Attribute] = ListBuffer.empty
    
	for (label <- matrix.labels) yield {
		val values = columnDomains(label)
		if (kFeatures.exists(_.name == label)) {
		  features += new Feature(label)
		} else {
		  val kAttribute = kAttributes.find(_.name == label).get

		  val nullValue = kAttribute.domain.nullValue 
		    
		  val lessThan = kAttribute.domain.lessThan
		  
		  val domain = new Domain(values, nullValue, lessThan)
		
		  attributes += new Attribute(label, domain)
		}
	  }

	  // FIXME : implicit features !
	  
	  (features.toList, attributes.toList)
  } 
  
  override def selectHierarchy(big : ImplicationGraph[Feature]) : ImplicationGraph[Feature] = {
    val kHierarchy = afm.diagram.hierarchy
    
    val hierarchyFinder = new OptimumBranchingFinder[Feature]
    val wbig = new WeightedImplicationGraph[Feature](big)
    for (edge <- wbig.edges()) {
      val source = wbig.getSource(edge)
      val target = wbig.getTarget(edge)
      
      val kSource = kHierarchy.vertices().find(_.name == source.name).get
      val kTarget = kHierarchy.vertices().find(_.name == target.name).get
      
      if (kHierarchy.containsEdge(kSource, kTarget)) {
    	  wbig.setEdgeWeight(edge, 1)
      }
      
    }
    
    hierarchyFinder.findOptimumBranching(wbig)
  }
  
  override def placeAttribute(attribute : Attribute, legalPositions : Set[Feature]) : Feature = {
    require(!legalPositions.isEmpty, "An attribute must have at least one possible place in the hierarchy")
    
    val enclosingFeature = afm.diagram.features.find(_.attributes.contains(attribute))
    val position = legalPositions.find(_.name == enclosingFeature.get.name)
    if (position.isDefined) {
    	position.get
    } else {
    	legalPositions.head
    }
  }
  
  override def selectOneGroup(overlappingGroups : Set[Relation]) : Relation = {
    // TODO : select groups w.r.t the AFM
    overlappingGroups.head
  }
  
  override def isTrue(feature : Feature, value : String) : Boolean = {
    // TODO : get the isTrue function from the AFM (which does not exist yet... :D ! )
    value == "1"
  }
}