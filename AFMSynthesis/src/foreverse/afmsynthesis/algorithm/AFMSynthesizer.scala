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
import scala.io.Source
import java.nio.file.Files
import java.io.File
import java.io.FileWriter
import com.github.tototoshi.csv.CSVWriter
import java.io.InputStream
import scala.sys.process._
import java.util.regex.Pattern
import foreverse.afmsynthesis.afm.AttributeValue
import foreverse.afmsynthesis.afm.AttributeValue

class AFMSynthesizer {
  
  
	def synthesize(matrix : ConfigurationMatrix, knowledge : Knowledge) : AttributedFeatureModel = {
	  
	  
	  // Extract the features, the attributes and their domains
	  val columnDomains = extractColumnDomains(matrix, knowledge)
	  
	  println("Domains")
	  columnDomains.foreach(d => println(d._1 + " => " + d._2))
	  println
	  
	  val (features, attributes) = extractFeaturesAndAttributes(matrix, columnDomains, knowledge)
	  val domains = (for (attribute <- attributes) yield {
	    (attribute.name -> attribute.domain)
	  }).toMap
	  
	  val root = new Feature("root", Nil, None, Nil)
	  
	  println("Features")
	  features.foreach(println)
	  println
	  
	  println("Attributes")
	  attributes.foreach(println)
	  println
	  
	  // Compute binary implications
	  val constraints = computeBinaryImplicationConstraints(matrix, features, attributes, columnDomains)
	  println("Constraints")
	  println(constraints.size)
	  constraints.foreach(println)
	  println
	  
	  // Define the hierarchy
	  val (big, mutexGraph) = computeBinaryImplicationAndMutexGraph(features, constraints)
	  println("BIG")
	  println(big.toDot)
	  println
	  val bigWriter = new FileWriter(new File("big.dot"))
	  bigWriter.write(big.toDot)
	  bigWriter.close()
	  
	  println("Mutex graph")
	  println(mutexGraph.toDot)
	  println
	  val mtxWriter = new FileWriter(new File("mtx.dot"))
	  mtxWriter.write(mutexGraph.toDot)
	  mtxWriter.close()
	  
	  
	  val hierarchy = extractHierarchy(big, knowledge)
	  
	  placeAttributes(root, features, attributes, constraints, knowledge)
	  
	  // Compute the variability information
	  
	  
	  // Compute constraints
	  
	  // Create the attributed feature model
	  val afd = new AttributedFeatureDiagram(features, root, Nil, Nil)
	  val phi = new CrossTreeConstraint
	  val afm = new AttributedFeatureModel(afd, phi)
	  afm
	}

	/**
	 * Extract domains of columns from the matrix
	 */
	def extractColumnDomains(matrix : ConfigurationMatrix, knowledge : Knowledge) : Map[String, Set[String]] = {
	  
	  val domains = collection.mutable.Map.empty[String, Set[String]]
		
	  for ((label, index) <- matrix.labels.zipWithIndex) {
	    
	    val values = (for (configuration <- matrix.configurations) yield {
		  configuration(index)
		}).toSet
		
		domains += (label -> values)
	  }
	  
	  domains.toMap
	}
	
	/**
	 * Extract features and attributes from the matrix
	 */
	def extractFeaturesAndAttributes(matrix : ConfigurationMatrix, domains : Map[String, Set[String]], knowledge : Knowledge) 
	: (List[Feature], List[Attribute]) = {
	  knowledge.extractFeaturesAndAttributes(matrix, domains)
	}

	/**
	 * Compute binary implications between the values of the matrix's columns
	 */
	def computeBinaryImplicationConstraints(matrix : ConfigurationMatrix, features : List[Feature], attributes : List[Attribute], columnDomains : Map[String, Set[String]])
	: List[BinaryImplicationConstraint] = {

	  // Create dictionary of matrix values
	  val dictionaries : collection.mutable.Map[String, Map[String, String]] = collection.mutable.Map.empty

	  for ((label, labelIndex) <- matrix.labels.zipWithIndex) yield {
	    val dictionary : collection.mutable.Map[String, String] = collection.mutable.Map.empty

	    val values = columnDomains(label)
	    val attribute = attributes.find(_.name == label)
	    val sortedValues = if (attribute.isDefined) {
	      values.toList.sortWith(attribute.get.domain.lessThan)
	    } else {
	      values.toList
	    }
	    
	    
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
	  
	  // Write converted matrix to CSV
	  val convertedMatrixFile = //File.createTempFile("afmsynthesis_", ".csv")
	    new File("convertedMatrix.csv")
	  val writer = new CSVWriter(new FileWriter(convertedMatrixFile))
	  writer.writeRow(convertedMatrix.labels)
	  convertedMatrix.configurations.foreach(writer.writeRow(_))
	  writer.close()

	  println(convertedMatrixFile.getAbsolutePath())
	  
	  // Compute binary implication constraints with a prolog reasoner
	  
	  // Run sicstus reasoner
	  // sicstus -f -l revfm.pl --goal main. -a configuration_matrix.csv results.txt
	  val reasonerCommand = Seq("sicstus", 
	      "-f", 
	      "-l", 
	      "sicstus_reasoner/revfm.pl", 
	      "--goal", 
	      "main.", 
	      "-a", 
	      convertedMatrixFile.getAbsolutePath(), 
	      "results.txt")
	      
	  val ioHandler = ProcessLogger(println, println)
	  val commandResult = reasonerCommand ! ioHandler
	  
	  assert(commandResult == 0, {convertedMatrixFile.delete(); "Something went wrong with Sicstus program"})

	  // Delete converted matrix file
//	  convertedMatrixFile.delete()

	  
	  // Parse the output of the reasoner  
	  // and convert it to a list of constraints over the features and attributes
	  val invertedDictionaries = dictionaries.map((kv) => (kv._1 -> kv._2.map(_.swap)))
	  
	  val pattern = Pattern.compile("Feat(\\d+)\\s=\\s(\\d+)\\s=>\\sFeat(\\d+)\\sin\\s\\[(.*)\\]\\sand\\snot\\sin\\s\\[(.*)\\]")
	  
	  val constraints = for (line <- Source.fromFile("results.txt").getLines) yield {
	    val matcher = pattern.matcher(line)
	    if (matcher.matches()) {
	    	
	    	val leftVariable = matcher.group(1)
		    val leftValue = matcher.group(2)
		    val rightVariable = matcher.group(3)
		    val impliedVariables = matcher.group(4).split(",").toList.filter(!_.isEmpty())
		    val excludedVariables = matcher.group(5).split(",").toList.filter(!_.isEmpty())
		    
		    println(leftVariable + " equals " + leftValue + " => " + rightVariable + " " + impliedVariables + " " + excludedVariables)
   
		    val value = convertVariableToValue(matrix.labels(leftVariable.toInt - 1), leftValue, features, attributes, invertedDictionaries)
	    	val implies = impliedVariables.map(impliedVariable => convertVariableToValue(matrix.labels(rightVariable.toInt - 1), impliedVariable, features, attributes, invertedDictionaries))
	    	val excludes = excludedVariables.map(impliedVariable => convertVariableToValue(matrix.labels(rightVariable.toInt - 1), impliedVariable, features, attributes, invertedDictionaries))
		    val constraint = new BinaryImplicationConstraint(value, implies, excludes)
	    	Some(constraint)
	    } else {
	    	println("Following line is not a correct output of Sicstus reasoner: " + line)
	    	None
	    }
	  }
	  
	  constraints.flatten.toList
	}
	
	private def convertVariableToValue(label : String, value : String, features : List[Feature], attributes : List[Attribute], invertedDictionaries : collection.mutable.Map[String, Map[String, String]])
	: Value = {
	  // FIXME : this conversion does not handle special case of implicit features (features not present in labels)
	  val originValue = invertedDictionaries(label)(value)
	  val feature = features.find(_.name == label)
	  if (feature.isDefined) {
		new FeatureValue(feature.get, originValue == "1") // FIXME : check that the string means true or false
	  } else {
		val attribute = attributes.find(_.name == label)
		new AttributeValue(attribute.get, originValue)
	  }
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
	    
		  val impliedValues = constraint.implies.flatMap(toFeatureValue(_))
		  if (impliedValues.size == 1 && impliedValues.head.positive) {
		    big.addEdge(source.get.feature, impliedValues.head.feature)
		  }
		  
		  val excludedValues = constraint.excludes.flatMap(toFeatureValue(_))
		  if (excludedValues.size == 1 && excludedValues.head.positive) {
		    mutexGraph.addEdge(source.get.feature, excludedValues.head.feature)
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
	
	def placeAttributes(root : Feature, features : List[Feature], attributes : List[Attribute], constraints : List[BinaryImplicationConstraint], knowledge : Knowledge) = {

	  // Compute legal positions for the attributes
	  val legalPositions = collection.mutable.Map.empty[Attribute, List[Feature]]
	  for (attribute <- attributes) {
	    legalPositions(attribute) = List(root)
	  }
	  
	  // If not f => a = 0d, then the feature f is a legal position for the attribute a 
	  for (constraint <- constraints) {
	    constraint.value match {
	      case FeatureValue(feature, positive) if !positive =>
	        for (implied <- constraint.implies) {
	          implied match {
	            case AttributeValue(attribute, value) if attribute.domain.nullValue == value =>
	              legalPositions(attribute) = feature :: legalPositions(attribute)
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
	
	def computeMandatoryFeatures(big : BinaryImplicationGraph) {
	  
	}
	
	
}