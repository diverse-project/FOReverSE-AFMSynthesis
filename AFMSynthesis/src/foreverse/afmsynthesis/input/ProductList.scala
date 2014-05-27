package foreverse.afmsynthesis.input

import foreverse.afmsynthesis.afm.Attribute
import foreverse.afmsynthesis.afm.Feature
import foreverse.afmsynthesis.synthesis.AFMSynthesisProblem

class ProductList (
	initFeatures : List[Feature],
	initAttributes : List[Attribute],
	private var products : List[List[Any]]    
) extends AFMSynthesisProblem(initFeatures, initAttributes) {
  
	override def removeDeadFeatures() {
		
	}

  
}