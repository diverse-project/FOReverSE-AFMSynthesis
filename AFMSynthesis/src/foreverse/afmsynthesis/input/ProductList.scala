package foreverse.afmsynthesis.input

import foreverse.afmsynthesis.afm.Attribute
import foreverse.afmsynthesis.afm.Feature
import foreverse.afmsynthesis.synthesis.AFMSynthesisProblem
import foreverse.afmsynthesis.afm.Domain
import foreverse.afmsynthesis.afm.VariationPoint
import foreverse.afmsynthesis.afm.VariationPoint

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

  
}