package foreverse.afmsynthesis.algorithm

import foreverse.afmsynthesis.afm.AttributedFeatureModel
import foreverse.afmsynthesis.afm.Feature
import foreverse.afmsynthesis.afm.Attribute
import foreverse.afmsynthesis.afm.AttributedFeatureDiagram
import foreverse.afmsynthesis.afm.CrossTreeConstraint
import foreverse.afmsynthesis.afm.Feature
import foreverse.afmsynthesis.afm.Feature
import foreverse.afmsynthesis.afm.domains.EnumDomain

class AFMSynthesizer {
  
  
	def synthesize(matrix : ConfigurationMatrix, knowledge : Any) : AttributedFeatureModel = {
	  
	  
	  val (features : List[Feature], attributes : List[Attribute]) = extractFeaturesAndAttributes(matrix, knowledge)
	  extractAttributeDomains(attributes, matrix, knowledge)
	  
	  
	  val root = new Feature("root", Nil, None, Nil)
	  val afd = new AttributedFeatureDiagram(features, root, Nil, Nil)
	  val phi = new CrossTreeConstraint
	  val afm = new AttributedFeatureModel(afd, phi)
	  afm
	}

	
	def extractFeaturesAndAttributes(matrix : ConfigurationMatrix, knowledge : Any) : (List[Feature], List[Attribute]) = {
		(Nil, Nil)
	}
	
	def extractAttributeDomains(attributes : List[Attribute], matrix : ConfigurationMatrix, knowledge : Any) {
		
	  for (attribute <- attributes;
		label <- matrix.labels.zipWithIndex
		if attribute == label._1) {
	    
	    val values = (for (configuration <- matrix.configurations) yield {
		  configuration(label._2)
		}).toSet
		
		// FIXME : determine domain type and null value according to the knowledge
		attribute.domain = EnumDomain(values, "N/A") 
		
	  }
	}
}