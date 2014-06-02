package foreverse.afmsynthesis.input

import foreverse.afmsynthesis.afm.VariationPoint
import foreverse.afmsynthesis.afm.VariationPoint
import foreverse.afmsynthesis.afm.VariationPoint

class ProductList(
	private var _variationPoints : List[VariationPoint],
	private var _products : List[List[Any]]
) {

	def variationPoints = _variationPoints
	
	def products = _products
	
	def removeVariationPoint(vp : VariationPoint) {
	  
	}
	
  
}