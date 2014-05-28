package foreverse.afmsynthesis.input

import foreverse.afmsynthesis.afm.Attribute
import foreverse.afmsynthesis.afm.Feature
import foreverse.afmsynthesis.synthesis.AFMSynthesisProblem
import foreverse.afmsynthesis.afm.Domain
import foreverse.afmsynthesis.afm.VariationPoint
import foreverse.afmsynthesis.afm.VariationPoint
import foreverse.afmsynthesis.synthesis.BinaryImplicationGraph
import foreverse.afmsynthesis.afm.Feature
import foreverse.afmsynthesis.afm.Feature

class ProductList (
    var featureByProductMatrix : Map[VariationPoint, List[Any]]
) extends AFMSynthesisProblem(Nil, Nil) {
  
	// separate features from attributes
	features = (for (variationPoint <- featureByProductMatrix.keys.toList) yield {
		variationPoint match {
		  case vp : Feature => Some(vp)
		  case _ => None
		}
	}).flatten
	
	attributes = (for (variationPoint <- featureByProductMatrix.keys.toList) yield {
		variationPoint match {
		  case vp : Attribute => Some(vp)
		  case _ => None
		}
	}).flatten
  
	
	override def removeDeadFeatures() {
		val liveFeatures = features.filter(f => !featureByProductMatrix(f).forall(_ == false))
		features = liveFeatures
		featureByProductMatrix = featureByProductMatrix.filter(e => liveFeatures.contains(e._1))
	}
	
	override def computeBinaryImplicationGraph() : BinaryImplicationGraph = {
		val big = new BinaryImplicationGraph
		
		big.addNodes(features)
		
		for (child <- features; parent <- features if child != parent) {
		  val products = featureByProductMatrix(child) zip featureByProductMatrix(parent)
		  if (!products.exists(p => p._1.asInstanceOf[Boolean] && !p._2.asInstanceOf[Boolean])) {
		    big.addEdge(child, parent)
		  }
		}
		big
	}
	
	
//	override def computeMutexGraph() : MutexGraph = {
//		val mg = new MutexGraph
//		
//		mg.addNodes(features)
//		
//		for (child <- features; parent <- features) {
//		  val products = featureByProductMatrix(child) zip featureByProductMatrix(parent)
//		  if (products.exists(p => p._1.asInstanceOf[Boolean] && !p._2.asInstanceOf[Boolean])) {
//		    big.addEdge(child, parent)
//		  }
//		}
//		mg
//	}

  
}