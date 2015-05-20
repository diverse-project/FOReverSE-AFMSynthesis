package model

import foreverse.afmsynthesis.afm.{Attribute, Relation, Feature}
import foreverse.afmsynthesis.algorithm.{DomainKnowledge, ConfigurationMatrix}

import scala.concurrent.Promise

/**
 * Created by gbecan on 3/24/15.
 */
class InteractiveDomainKnowledge extends DomainKnowledge {

  override def extractFeaturesAndAttributes(matrix: ConfigurationMatrix, columnDomains: Map[String, Set[String]]): (List[Feature], List[Attribute]) = {
    // TODO : use future to get the result
//    var toto : (List[Feature], List[Attribute]) = (Nil, Nil)
//
//    extractFeaturesAndAttributesAsync().future.onSuccess {
//      e : (List[Feature], List[Attribute]) => println(e)
//    }
    (Nil, Nil)
  }

//  def extractFeaturesAndAttributesAsync() : Promise[(List[Feature], List[Attribute])] {
//
//  }

  override def placeAttribute(attribute: Attribute, legalPositions: Set[Feature]): Feature = ???

  override def getConstraintBound(attribute: Attribute): String = ???

  override def selectHierarchy(big: _root_.gsd.graph.ImplicationGraph[Feature]): _root_.gsd.graph.ImplicationGraph[Feature] = ???

  override def selectOneGroup(overlappingGroups: Set[Relation]): Relation = ???

  override def isTrue(feature: Feature, value: String): Boolean = ???
}
