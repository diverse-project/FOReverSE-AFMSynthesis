package foreverse.afmsynthesis.synthesis

import scalax.collection.mutable.Graph
import foreverse.afmsynthesis.afm.Feature
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.io.dot._

class BinaryImplicationGraph {

	private val graph : Graph[Feature, DiEdge] = Graph.empty
		
	def addNode(node : Feature) {
	  	graph += node
	}
	
	def addNodes(nodes : List[Feature]) {
		graph ++= nodes
	}
	
	def addEdge(source : Feature, target : Feature) {
		graph += (source~>target)
	}
	
	override def toString() : String = {
	  graph.toString
	}
	
	def toDot() : String = {
	  val root = DotRootGraph(directed=true, id=None)
	  def edgeTransformer(innerEdge: scalax.collection.Graph[Feature,DiEdge]#EdgeT): Option[(DotGraph,DotEdgeStmt)] = {
		  val edge = innerEdge.edge
		  Some(root, DotEdgeStmt(edge.from.toString, edge.to.toString))
	  }
	  
	  val dot = graph.toDot(root, edgeTransformer)
	  dot
	}
  
}