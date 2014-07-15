package foreverse.afmsynthesis.afm

import scalax.collection.mutable.Graph
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.io.dot._


class MutexGraph {

  private val graph : Graph[Feature, UnDiEdge] = Graph.empty
 
  
  def addNode(node : Feature) {
	  	graph += node
	}
	
	def addNodes(nodes : List[Feature]) {
		graph ++= nodes
	}
	
	def addEdge(source : Feature, target : Feature) {
		graph += (source~target)
	}
	
	def toDot() : String = {
	  val root = DotRootGraph(directed=false, id=None)
	  def edgeTransformer(innerEdge: scalax.collection.Graph[Feature,UnDiEdge]#EdgeT): Option[(DotGraph,DotEdgeStmt)] = {
		  val edge = innerEdge.edge
		  Some(root, DotEdgeStmt(edge._1.toString, edge._2.toString)) 
	  }
	  
	   def iNodeTransformer(node : scalax.collection.Graph[Feature,UnDiEdge]#NodeT) : Option[(DotGraph, DotNodeStmt)] = {
	    Some(root, DotNodeStmt(node.toString, Seq.empty[DotAttr]))
	  }
	  
	  val dot = graph.toDot(root, edgeTransformer, iNodeTransformer=Some(iNodeTransformer))
	  dot
	}
	
}