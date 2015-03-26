package foreverse.afmsynthesis.algorithm

import foreverse.afmsynthesis.afm.{Attribute, Feature, Relation}
import gsd.graph.ImplicationGraph

trait DomainKnowledge {

  def extractFeaturesAndAttributes(matrix : ConfigurationMatrix, columnDomains : Map[String, Set[String]]) : (List[Feature], List[Attribute]) 
  
  def selectHierarchy(big : ImplicationGraph[Feature]) : ImplicationGraph[Feature] 
  
  def placeAttribute(attribute : Attribute, legalPositions : Set[Feature]) : Feature
  
  def selectOneGroup(overlappingGroups : Set[Relation]) : Relation 
  
  def isTrue(feature : Feature, value : String) : Boolean 
  
  def getConstraintBound(attribute : Attribute) : String
  
}