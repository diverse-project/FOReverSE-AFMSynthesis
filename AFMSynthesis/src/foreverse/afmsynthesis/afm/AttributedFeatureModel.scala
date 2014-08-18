package foreverse.afmsynthesis.afm

import foreverse.afmsynthesis.afm.constraint.CrossTreeConstraint

class AttributedFeatureModel(
	val diagram : AttributedFeatureDiagram,
	val constraint : Option[CrossTreeConstraint]
) {

  
}