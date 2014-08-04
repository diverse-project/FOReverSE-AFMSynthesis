package foreverse.afmsynthesis.algorithm

import scalax.collection.mutable.Graph
import foreverse.afmsynthesis.afm.Feature
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.io.dot._
import scalax.collection.mutable.DefaultGraphImpl
import scala.reflect.runtime.universe._

class BinaryImplicationGraph {// extends DefaultGraphImpl[Feature, DiEdge](Set.empty, Set.empty)(typeTag[DiEdge[Feature]], Graph.defaultConfig) {

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
	
	def toDot() : String = {
	  val root = DotRootGraph(directed=true, id=None)
	  def edgeTransformer(innerEdge: scalax.collection.Graph[Feature,DiEdge]#EdgeT): Option[(DotGraph,DotEdgeStmt)] = {
		  val edge = innerEdge.edge
		  Some(root, DotEdgeStmt(edge.from.value.name, edge.to.value.name))
	  }
	  
	  def iNodeTransformer(node : scalax.collection.Graph[Feature,DiEdge]#NodeT) : Option[(DotGraph, DotNodeStmt)] = {
	    Some(root, DotNodeStmt(node.value.name, Seq.empty[DotAttr]))
	  }
	  
	  val dot = graph.toDot(root, edgeTransformer, iNodeTransformer=Some(iNodeTransformer))
	  dot
	}
  
}