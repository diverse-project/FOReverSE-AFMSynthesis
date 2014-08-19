package foreverse.afmsynthesis.afm

import scalax.collection.mutable.Graph
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.edge.LDiHyperEdge

class Hierarchy {
  
  private val graph : Graph[Feature, LDiHyperEdge] = Graph.empty

  def addNode(node : Feature) {
	graph += node
  }
	
  def addNodes(nodes : List[Feature]) {
	graph ++= nodes
  }
  
  
  
}