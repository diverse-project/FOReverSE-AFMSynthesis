package foreverse.afmsynthesis.algorithm

import java.io.File
import java.io.FileWriter
import java.util.regex.Pattern
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.io.Source
import scala.sys.process.ProcessLogger
import scala.sys.process.stringSeqToProcess
import com.github.tototoshi.csv.CSVWriter
import foreverse.afmsynthesis.afm.Attribute
import foreverse.afmsynthesis.afm.AttributeValue
import foreverse.afmsynthesis.afm.AttributedFeatureDiagram
import foreverse.afmsynthesis.afm.AttributedFeatureModel
import foreverse.afmsynthesis.afm.BinaryImplicationConstraint
import foreverse.afmsynthesis.afm.CrossTreeConstraint
import foreverse.afmsynthesis.afm.Feature
import foreverse.afmsynthesis.afm.FeatureValue
import foreverse.afmsynthesis.afm.Knowledge
import foreverse.afmsynthesis.afm.Value
import gsd.graph.ImplicationGraph
import scala.collection.JavaConversions._
import gsd.graph.DirectedCliqueFinder
import gsd.fms.sat.MutexGroupFinder
import fr.familiar.fm.converter.ExclusionGraph
import gsd.graph.GraphvizGraph
import gsd.graph.SimpleEdge
import gsd.graph.BasicGraph
import fr.familiar.operations.ExclusionGraphUtil
import scala.collection.mutable.ListBuffer
import foreverse.afmsynthesis.afm.MutexGroup
import foreverse.afmsynthesis.afm.MutexGroup
import foreverse.afmsynthesis.afm.Mandatory
import foreverse.afmsynthesis.afm.OrGroup
import foreverse.afmsynthesis.afm.XorGroup
import foreverse.afmsynthesis.afm.MutexGroup
import foreverse.afmsynthesis.afm.OrGroup
import foreverse.afmsynthesis.afm.XorGroup
import foreverse.afmsynthesis.afm.XorGroup
import dk.itu.fms.formula.dnf.DNF
import dk.itu.fms.formula.dnf.DNFClause
import foreverse.afmsynthesis.afm.OrGroup
import foreverse.afmsynthesis.afm.OrGroup

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
//	  constraints.foreach(println)
	  println
	  
	  // Define the hierarchy
	  val (big, mutexGraph) = computeBinaryImplicationAndMutexGraph(features, constraints)
	  
	  println("BIG")
	  println(big.toString())
	  println
	  val bigWriter = new FileWriter(new File("output/big.dot"))
	  bigWriter.write(big.toString())
	  bigWriter.close()
	  
	  println("Mutex graph")
	  println(mutexGraph)
	  println
//	  val mtxWriter = new FileWriter(new File("output/mtx.dot"))
//	  mtxWriter.write(mutexGraph.toString())
//	  mtxWriter.close()
	  
	  
	  val hierarchy = extractHierarchy(big, knowledge)
	  
	  println("Hierarchy")
	  println(hierarchy)
	  println()
	  val hWriter = new FileWriter(new File("output/h.dot"))
	  hWriter.write(hierarchy.toString())
	  hWriter.close()
	  
	  placeAttributes(features, attributes, constraints, knowledge)
	  
	  println("Features with attributes")
	  features.foreach(println)
	  println
	  
	  // Compute variability information

	  val mandatoryRelations = computeMandatoryFeatures(big, hierarchy)
	  
	  println("Mandatory relations")
	  mandatoryRelations.foreach(println)
	  println()
	  
	  val mutexGroups = computeMutexGroups(mutexGraph, hierarchy, features)
	  println("Mutex groups")
	  mutexGroups.foreach(println)
	  println()
	  
	  val orGroups = computeOrGroups(matrix, hierarchy, features)
	  println("Or groups")
	  orGroups.foreach(println)
	  println()
	  
	  val xorGroups = computeXOrGroups(mutexGroups, orGroups)
	  println("XOr groups")
	  xorGroups.foreach(println)
	  println() 
	  
	  // Compute constraints
	  
	  // Create the attributed feature model
	  val root = hierarchy.roots().iterator().next()
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
	    new File("output/convertedMatrix.csv")
	  val writer = new CSVWriter(new FileWriter(convertedMatrixFile))
	  writer.writeRow(convertedMatrix.labels)
	  convertedMatrix.configurations.foreach(writer.writeRow(_))
	  writer.close()

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
	      "output/results.txt")
	      
	  val ioHandler = ProcessLogger(_ => {}, _ => {})
	  val commandResult = reasonerCommand ! ioHandler
	  
	  assert(commandResult == 0, {convertedMatrixFile.delete(); "Something went wrong with Sicstus program"})

	  // Delete converted matrix file
//	  convertedMatrixFile.delete()

	  
	  // Parse the output of the reasoner  
	  // and convert it to a list of constraints over the features and attributes
	  val invertedDictionaries = dictionaries.map((kv) => (kv._1 -> kv._2.map(_.swap)))
	  
	  val pattern = Pattern.compile("Feat(\\d+)\\s=\\s(\\d+)\\s=>\\sFeat(\\d+)\\sin\\s\\[(.*)\\]\\sand\\snot\\sin\\s\\[(.*)\\]")
	  
	  val constraints = for (line <- Source.fromFile("output/results.txt").getLines) yield {
	    val matcher = pattern.matcher(line)
	    if (matcher.matches()) {
	    	
	    	val leftVariable = matcher.group(1)
		    val leftValue = matcher.group(2)
		    val rightVariable = matcher.group(3)
		    val impliedVariables = matcher.group(4).split(",").toList.filter(!_.isEmpty())
		    val excludedVariables = matcher.group(5).split(",").toList.filter(!_.isEmpty())
		    
//		    println(leftVariable + " equals " + leftValue + " => " + rightVariable + " " + impliedVariables + " " + excludedVariables)
   
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
	: (ImplicationGraph[Feature], ExclusionGraph[Feature]) = {
	  
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
	  
//	  val big = new BinaryImplicationGraph
//	  big.addNodes(features)
	  
	  val big = new ImplicationGraph[Feature]
	  features.foreach(big.addVertex(_))
	  
//	  val mutexGraph = new MutexGraph
//	  mutexGraph.addNodes(features)
	  
	  val mutexGraph = new ExclusionGraph[Feature]
	  features.foreach(mutexGraph.addVertex(_))
	  
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
	  
//	  // Add root to implication graph
//	  val topCliques = big.reduceCliques().roots()
//	  
//	  big.addVertex(root)
//	  features.foreach(big.addEdge(_, root))
//	  
//	  // add edges from root to top features 
//	  // which are mandatory because the root is not part of the matrix 
//	  for (topClique <- topCliques;
//	      topFeature <- topClique) {
//		  big.addEdge(root, topFeature)
//	  }
	  
	  
	  (big, mutexGraph)
	}
	
	/**
	 * Extract a particular hierarchy for the AFM
	 * @param : big : binary implication graph representing all legal hierarchies of the AFM
	 */
	def extractHierarchy(big : ImplicationGraph[Feature], knowledge : Knowledge): ImplicationGraph[Feature] = {
	  knowledge.selectHierarchy(big)
	}
	
	def placeAttributes(features : List[Feature], attributes : List[Attribute], constraints : List[BinaryImplicationConstraint], knowledge : Knowledge) = {

	  // Compute legal positions for the attributes
	  val legalPositions = collection.mutable.Map.empty[Attribute, Set[Feature]]
	  for (attribute <- attributes) {
	    legalPositions(attribute) = features.toSet
	  }
	  
	  // If (not f => a = 0d), then the feature f is a legal position for the attribute a
	  // here we check the negation of this property to remove illegal positions
	  for (constraint <- constraints) {
	    constraint.value match {
	      case FeatureValue(feature, positive) if !positive=>
	        for (implied <- constraint.implies) {
	          implied match {
	            case AttributeValue(attribute, value) if attribute.domain.nullValue == value =>
	              legalPositions(attribute) = legalPositions(attribute) - feature
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
	
	def computeMandatoryFeatures(big : ImplicationGraph[Feature], hierarchy : ImplicationGraph[Feature])
	: List[Mandatory] = {
	  val mandatoryRelations = ListBuffer.empty[Mandatory]
	  
	  for (edge <- hierarchy.edges()) {
	    val child = hierarchy.getSource(edge)
		val parent = hierarchy.getTarget(edge)
		
		if (Option(big.findEdge(parent, child)).isDefined) {
		  mandatoryRelations += Mandatory(parent, child)
		}
	  }
	  
	  mandatoryRelations.toList
	}
	
	def computeMutexGroups(mutexGraph : ExclusionGraph[Feature], hierarchy : ImplicationGraph[Feature], features : List[Feature])
	: List[MutexGroup] = {
	  val mutexGroups = ListBuffer.empty[MutexGroup]
	  val cliques = new ExclusionGraphUtil[Feature].cliques(mutexGraph)
	  for (parent <- features; clique <- cliques) {
	    val children = clique intersect hierarchy.children(parent)
	    if (children.size >= 2) {
	      mutexGroups += MutexGroup(parent, children.toList)
	    }
	  }
	  
	  mutexGroups.toList
	}
	
	
	def computeOrGroups(matrix : ConfigurationMatrix, hierarchy : ImplicationGraph[Feature], features : List[Feature]) 
	: List[OrGroup] = {
	  
	  // Convert matrix to DNF
	  val variables = features.map(feature => matrix.labels.indexOf(feature.name))  
	  
	  val clauses = for (configuration <- matrix.configurations) yield {
		  val clause = new Array[Int](variables.size)
		  for ((variable, index) <- variables.zipWithIndex) {
			  val value = configuration(variable) // FIXME : convert string of the matrix to boolean
			  
			  // a literal must not be equal to 0
			  val literal = if (value == "1") {
			    variable + 1
			  }  else {
			    - (variable + 1)
			  }
			  clause(index) = literal
		  }

		  new DNFClause(clause)
	  }
	  
	  val dnf = new DNF(clauses)
	  
	  // Compute Or groups
	  val orGroups = ListBuffer.empty[OrGroup]
	  for (variable <- variables) {
	    val computedOrGroups = dnf.getOrGroups(variable)
	    
	    for (orGroup <- computedOrGroups) {
	      val literals = orGroup.getLiterals()
	      val children = for (literal <- literals) yield {
	        val label = matrix.labels(literal.toInt.abs - 1)
	        val feature = features.find(_.name == label).get
	        feature
	      }
	      val parent = features.find(_.name == matrix.labels(variable)).get
	      
	      // Filter groups that are not possible with this hierarchy
	      if (hierarchy.children(parent).containsAll(children)) {
	    	  orGroups += OrGroup(parent, children.toList)
	      }
	      
	    }
	  }
 
	  orGroups.toList
	}
	
	def computeXOrGroups(mutexGroups : List[MutexGroup], orGroups : List[OrGroup]) : List[XorGroup] = {
	  val xorGroups = ListBuffer.empty[XorGroup]
	
	  for (mtxG <- mutexGroups) {
	    val isOr = orGroups.exists(orG => mtxG.parent == orG.parent && mtxG.children == orG.children)
	    if (isOr) {
	      xorGroups += XorGroup(mtxG.parent, mtxG.children)
	    }
	  }
	    
	  xorGroups.toList
	}
}