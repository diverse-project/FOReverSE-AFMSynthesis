package foreverse.afmsynthesis.algorithm

import scala.Array.canBuildFrom
import foreverse.afmsynthesis.afm.Attribute
import foreverse.afmsynthesis.afm.AttributedFeatureDiagram
import foreverse.afmsynthesis.afm.AttributedFeatureModel
import foreverse.afmsynthesis.afm.CrossTreeConstraint
import foreverse.afmsynthesis.afm.Domain
import foreverse.afmsynthesis.afm.Feature
import foreverse.afmsynthesis.afm.Knowledge
import foreverse.afmsynthesis.afm.BinaryImplicationConstraint
import foreverse.afmsynthesis.afm.BinaryImplicationConstraint
import foreverse.afmsynthesis.afm.FeatureValue
import foreverse.afmsynthesis.afm.AttributeValue
import foreverse.afmsynthesis.afm.FeatureValue
import foreverse.afmsynthesis.afm.Value
import foreverse.afmsynthesis.afm.FeatureValue
import foreverse.afmsynthesis.afm.MutexGraph
import foreverse.afmsynthesis.afm.FeatureValue
import foreverse.afmsynthesis.afm.AttributeValue

class AFMSynthesizer {
  
  
	def synthesize(matrix : ConfigurationMatrix, knowledge : Knowledge) : AttributedFeatureModel = {
	  
	  
	  val domains = extractDomains(matrix, knowledge)
	  
	  println("Domains")
	  domains.foreach(d => println(d._1 + " => " + d._2))
	  println
	  
	  val (features, attributes) = extractFeaturesAndAttributes(matrix, domains, knowledge)
	  
	  println("Features")
	  features.foreach(println)
	  println
	  
	  println("Attributes")
	  attributes.foreach(println)
	  println
	  
	  
	  val constraints = computeBinaryImplicationConstraints(matrix, features, attributes, domains)
	  println("Constraints")
	  constraints.foreach(println)
	  println
	  
	  val (big, mutexGraph) = computeBinaryImplicationAndMutexGraph(features, constraints)
	  println("BIG")
	  println(big.toDot)
	  println
	  
	  println("Mutex graph")
	  println(mutexGraph.toDot)
	  println
	  
	  
	  val hierarchy = extractHierarchy(big, knowledge)
	  
	  val root = new Feature("root", Nil, None, Nil)
	  val afd = new AttributedFeatureDiagram(features, root, Nil, Nil)
	  val phi = new CrossTreeConstraint
	  val afm = new AttributedFeatureModel(afd, phi)
	  afm
	}

	/**
	 * Extract domains of columns from the matrix
	 */
	def extractDomains(matrix : ConfigurationMatrix, knowledge : Knowledge) : Map[String, Domain] = {
	  
	  val domains : collection.mutable.Map[String, Domain] = collection.mutable.Map.empty
		
	  for (label <- matrix.labels.zipWithIndex) {
	    
	    val values = (for (configuration <- matrix.configurations) yield {
		  configuration(label._2)
		}).toSet
		
		// FIXME : determine null value according to the knowledge
		val nullValue = "" 
		
		// FIXME : determine (partial) order according to the knowledge
		val inferior = (a : String, b : String) => false 
		
		val domain = new Domain(values, nullValue, inferior)
		
		domains += (label._1 -> domain)
	  }
	  
	  domains.toMap
	}
	
	/**
	 * Extract features and attributes from the matrix
	 */
	def extractFeaturesAndAttributes(matrix : ConfigurationMatrix, domains : Map[String, Domain], knowledge : Knowledge) 
	: (List[Feature], List[Attribute]) = {
	  knowledge.extractFeaturesAndAttributes(matrix, domains)
	}

	/**
	 * Compute binary implications between the values of the matrix's columns
	 */
	def computeBinaryImplicationConstraints(matrix : ConfigurationMatrix, features : List[Feature], attributes : List[Attribute], domains : Map[String, Domain])
	: List[BinaryImplicationConstraint] = {

	  // Create dictionary of matrix values
	  val dictionaries : collection.mutable.Map[String, Map[String, String]] = collection.mutable.Map.empty

	  for ((label, labelIndex) <- matrix.labels.zipWithIndex) yield {
	    val dictionary : collection.mutable.Map[String, String] = collection.mutable.Map.empty

	    val columnDomain = domains(label)
	    val values = columnDomain.values.toList.sortWith(columnDomain.lessThan)
	    
	    for ((value, valueIndex) <- values.zipWithIndex) {
	    	dictionary += (value -> valueIndex.toString)
	    }
	    
	    dictionaries += (label -> dictionary.toMap) 		
	  }
	  
	  // Convert matrix to reasoner format
	  val convertedConfigurations = matrix.configurations.map(configuration =>
	    for ((value, index) <- configuration.zipWithIndex) yield {
	      dictionaries(matrix.labels(index))(value)
	    }
	  )
	  
	  val convertedMatrix = new ConfigurationMatrix(matrix.labels, convertedConfigurations)
	  
	  // Compute binary implication constraints with a prolog reasoner
	  // TODO : run sicstus
	  
	    
	  // Convert the output of the reasoner to a list of constraints over the features and attributes
	  val invertedDictionaries = dictionaries.map((kv) => (kv._1 -> kv._2.map(_.swap)))
	  // TODO : get back the original values
	  // TODO : map the values to either features or attribute values
	  	  
	  Nil
	}
	
	/**
	 * Compute binary implication graph and mutex graph
	 */
	def computeBinaryImplicationAndMutexGraph(features : List[Feature], constraints : List[BinaryImplicationConstraint])
	: (BinaryImplicationGraph, MutexGraph) = {
	  
	  def toFeatureValue(value : Value) : Option[FeatureValue] = {
	    value match {
	      case FeatureValue(feature, positive) => Some(FeatureValue(feature, positive))
	      case AttributeValue(_, _) => None
	    }
	  }
	  
	  def iterateOverPositiveValues(values : List[Value])(body : FeatureValue => Unit) = {
	     for (value <- values;
	        featureValue = toFeatureValue(value)
	        if featureValue.isDefined && featureValue.get.positive
	        ) {
	       body(featureValue.get)
	     }
	  }
	  
	  val big = new BinaryImplicationGraph
	  big.addNodes(features)
	  
	  val mutexGraph = new MutexGraph
	  mutexGraph.addNodes(features)
	  
	  for (constraint <- constraints;
		  source = toFeatureValue(constraint.value)
	      if source.isDefined && source.get.positive) {
	    
	   	  iterateOverPositiveValues(constraint.implies) {
	   		  target => big.addEdge(source.get.feature, target.feature)
	   	  }

	   	  iterateOverPositiveValues(constraint.excludes) {
	   		  target => mutexGraph.addEdge(source.get.feature, target.feature)
	   	  }
	  }
	  
	  (big, mutexGraph)
	}
	
	/**
	 * Extract a particular hierarchy for the AFM
	 * @param : big : binary implication graph representing all legal hierarchies of the AFM
	 */
	def extractHierarchy(big : BinaryImplicationGraph, knowledge : Knowledge) = {
	  // TODO : define return type
	  knowledge.selectHierarchy(big)
	}
	
	def placeAttributes(features : List[Feature], attributes : List[Attribute], constraints : List[BinaryImplicationConstraint], knowledge : Knowledge) = {

	  // Compute legal positions for the attributes
	  val legalPositions = collection.mutable.Map.empty[Attribute, List[Feature]]
	  
	  // If not f => a = 0d, then the feature f is a legal position for the attribute a 
	  for (constraint <- constraints) {
	    constraint.value match {
	      case FeatureValue(feature, positive) if !positive =>
	        for (implied <- constraint.implies) {
	          implied match {
	            case AttributeValue(attribute, value) if attribute.domain.nullValue == value =>
	              legalPositions += attribute -> (feature :: legalPositions.getOrElse(attribute, Nil))
	            case _ =>
	          }
	        }
	      case _ =>
	    }
	  }

	  // Choose a position for each attribute
	  for (attribute <- attributes) {
	    val selectedFeature = knowledge.placeAttribute(attribute, legalPositions(attribute))
	    selectedFeature.attributes = attribute :: selectedFeature.attributes
	  }

	}
	
}