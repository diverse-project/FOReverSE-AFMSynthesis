package foreverse.afmsynthesis.synthesis

import foreverse.afmsynthesis.afm.AttributedFeatureModel
import foreverse.afmsynthesis.afm.AttributedFeatureModel
import foreverse.afmsynthesis.afm.AttributedFeatureDiagram


class AFMSynthesizer {

	def synthesize(synthesisProblem : AFMSynthesisProblem) : AttributedFeatureModel = {
		
		val afm = new AttributedFeatureModel
		val afd = new AttributedFeatureDiagram
	  
		synthesisProblem.removeDeadFeatures
//		synthesisProblem.computeBinaryImplicationGraph
//		// set hierarchy or compute DAG
//		synthesisProblem.computeMutexGraph
//		synthesisProblem.computeMutexGroups
//		synthesisProblem.computeOrGroups
//		synthesisProblem.computeXOrGroups
//		synthesisProblem.computePossibleFeaturesForAttributes
//		// set hierarchy + groups + attributes
//		synthesisProblem.computeReadableConstraints

		afd.features = synthesisProblem.features
		
		afm.diagram = afd
		afm
	}

}