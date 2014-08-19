package foreverse.afmsynthesis.afm

import foreverse.afmsynthesis.algorithm.ConfigurationMatrix
import gsd.graph.ImplicationGraph

trait DomainKnowledge {

  def extractFeaturesAndAttributes(matrix : ConfigurationMatrix, columnDomains : Map[String, Set[String]]) : (List[Feature], List[Attribute]) 
  
  def selectHierarchy(big : ImplicationGraph[Feature]) : ImplicationGraph[Feature] 
  
  def placeAttribute(attribute : Attribute, legalPositions : Set[Feature]) : Feature
  
  def selectOneGroup(overlappingGroups : Set[Relation]) : Relation 
  
  def isTrue(feature : Feature, value : String) : Boolean 
  
}