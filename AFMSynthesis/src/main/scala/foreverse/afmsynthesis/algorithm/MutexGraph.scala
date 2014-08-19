package foreverse.afmsynthesis.algorithm

import scalax.collection.mutable.Graph
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.io.dot._
import foreverse.afmsynthesis.afm.Feature
import scala.reflect.runtime.universe
import scalax.collection.mutable.Graph.empty$default$2


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
		  Some(root, DotEdgeStmt(edge._1.value.name, edge._2.value.name)) 
	  }
	  
	   def iNodeTransformer(node : scalax.collection.Graph[Feature,UnDiEdge]#NodeT) : Option[(DotGraph, DotNodeStmt)] = {
	    Some(root, DotNodeStmt(node.value.name, Seq.empty[DotAttr]))
	  }
	  
	  val dot = graph.toDot(root, edgeTransformer, iNodeTransformer=Some(iNodeTransformer))
	  dot
	}
	
}