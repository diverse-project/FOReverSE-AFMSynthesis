package foreverse.afmsynthesis.algorithm

import java.io.File
import java.io.FileWriter
import java.util.regex.Pattern
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.collection.JavaConversions.asScalaSet
import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.JavaConversions.mutableSetAsJavaSet
import scala.collection.JavaConversions.seqAsJavaList
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.sys.process.ProcessLogger
import scala.sys.process.stringSeqToProcess
import org.jgrapht.alg.TransitiveClosure
import com.github.tototoshi.csv.CSVWriter
import dk.itu.fms.formula.dnf.DNF
import dk.itu.fms.formula.dnf.DNFClause
import foreverse.afmsynthesis.afm.Attribute
import foreverse.afmsynthesis.afm.AttributedFeatureDiagram
import foreverse.afmsynthesis.afm.AttributedFeatureModel
import foreverse.afmsynthesis.afm.Feature
import foreverse.afmsynthesis.afm.FeatureGroup
import foreverse.afmsynthesis.afm.Knowledge
import foreverse.afmsynthesis.afm.Mandatory
import foreverse.afmsynthesis.afm.MutexGroup
import foreverse.afmsynthesis.afm.OrGroup
import foreverse.afmsynthesis.afm.Relation
import foreverse.afmsynthesis.afm.XorGroup
import foreverse.afmsynthesis.afm.constraint.AttributeOperator
import foreverse.afmsynthesis.afm.constraint.AttributeValue
import foreverse.afmsynthesis.afm.constraint.BinaryExclusionConstraint
import foreverse.afmsynthesis.afm.constraint.BinaryImplicationConstraint
import foreverse.afmsynthesis.afm.constraint.Constraint
import foreverse.afmsynthesis.afm.constraint.Equal
import foreverse.afmsynthesis.afm.constraint.FeatureValue
import foreverse.afmsynthesis.afm.constraint.Not
import foreverse.afmsynthesis.afm.constraint.Variable
import foreverse.afmsynthesis.test.PerformanceMonitor
import fr.familiar.fm.converter.ExclusionGraph
import fr.familiar.operations.ExclusionGraphUtil
import gsd.graph.ImplicationGraph
import foreverse.afmsynthesis.afm.constraint.Equal
import foreverse.afmsynthesis.afm.constraint.Implies
import foreverse.afmsynthesis.afm.Attribute
import foreverse.afmsynthesis.afm.constraint.True
import foreverse.afmsynthesis.afm.constraint.IncludedIn
import foreverse.afmsynthesis.afm.constraint.IncludedIn
import foreverse.afmsynthesis.afm.constraint.And
import foreverse.afmsynthesis.afm.constraint.Implies

class AFMSynthesizer extends PerformanceMonitor {
  
  
	def synthesize(matrix : ConfigurationMatrix, knowledge : Knowledge) : AttributedFeatureModel = {
	  reset() // Reset performance monitor
	  start("Synthesis")
	  
	  // Extract the features, the attributes and their domains
	  start("Domain extraction")
	  val columnDomains = extractColumnDomains(matrix, knowledge)
	  stopLast()
	  
	  println("Domains")
//	  columnDomains.foreach(d => println(d._1 + " => " + d._2))
//	  println
	
	  start("Feature and attribute extraction")
	  val (features, attributes) = extractFeaturesAndAttributes(matrix, columnDomains, knowledge)
	  val domains = (for (attribute <- attributes) yield {
	    (attribute.name -> attribute.domain)
	  }).toMap
	  stopLast()
	  
	  println("Features")
//	  features.foreach(println)
	  println
	  
	  println("Attributes")
//	  attributes.foreach(println)
	  println
	  
	  // Compute binary implications
	  start("Binary implications")
	  val constraints = computeBinaryImplicationConstraints(matrix, features, attributes, columnDomains, knowledge)
	  constraints.foreach(println)
	  stopLast()
	  
	  println("Constraints")
	  println(constraints.size)
//	  constraints.foreach(println)
	  println
	  
	  // Define the hierarchy
	  start("Implication and Mutex graph")
	  val (big, mutexGraph) = computeBinaryImplicationAndMutexGraph(features, constraints)
	  stopLast()
	  
	  println("BIG")
//	  println(big.toString())
//	  println
//	  val bigWriter = new FileWriter(new File("output/big.dot"))
//	  bigWriter.write(big.toString())
//	  bigWriter.close()

	  println("Mutex graph")
//	  println(mutexGraph)
//	  println
	  
	  
	  start("Hierarchy")
	  val hierarchy = extractHierarchy(big, knowledge)
	  stopLast()
	  
	  println("Hierarchy")
//	  println(hierarchy)
//	  println()
//	  val hWriter = new FileWriter(new File("output/h.dot"))
//	  hWriter.write(hierarchy.toString())
//	  hWriter.close()
	  
	  start("Place attributes")
	  placeAttributes(features, attributes, constraints, knowledge)
	  stopLast()
	  
	  println("Features with attributes")
//	  features.foreach(println)
	  println
	  
	  // Compute variability information

	  start("Mandatory features")
	  val mandatoryRelations = computeMandatoryFeatures(big, hierarchy)
	  stopLast()
	  
	  println("Mandatory relations")
	  mandatoryRelations.foreach(println)
	  println()
	  
	  start("Feature groups")
	  start("Mutex")
	  var mutexGroups = computeMutexGroups(mutexGraph, hierarchy, features)
	  stopLast()
	  
	  start("Or")
	  var orGroups = computeOrGroups(matrix, hierarchy, features, knowledge)
	  stopLast()
	  
	  start("Xor")
	  var xorGroups = computeXOrGroups(mutexGroups, orGroups)
	  stopLast()
	  
	  start("Group processing")
	  val selectedGroups = processOverlappingGroups(features, mutexGroups, orGroups, xorGroups, knowledge) 
	  mutexGroups = selectedGroups._1
	  orGroups = selectedGroups._2
	  xorGroups = selectedGroups._3
	  stopLast()
	  
	  stopLast() // End feature group computation
	  
	  println("Mutex groups")
	  mutexGroups.foreach(println)
	  println()
	  
	  println("Or groups")
	  orGroups.foreach(println)
	  println()
	  
	  println("Xor groups")
	  xorGroups.foreach(println)
	  println() 
	  
	  
	  
	  // Compute constraints
	  start("Cross tree constraints")
	  start("Binary implies")
	  val implies = computeCrossTreeImplications(hierarchy, big, mandatoryRelations)
	  stopLast()
	  
	  start("Binary excludes")
	  val excludes = computeCrossTreeExcludes(mutexGraph, mutexGroups, xorGroups)
	  stopLast()
	  
	  
	  start("Complex constraints")
	  val complexConstraints = computeComplexCrossTreeConstraints(constraints)
	  stopLast()
	  val rc = implies ::: excludes 
	  stopLast()
	  
	  println("Constraints")
	  println(rc.size)
	  rc.foreach(println)
	  println()
	  
	  // Create the attributed feature model
	  start("AFM construction")
	  val afd = new AttributedFeatureDiagram(features, hierarchy, mandatoryRelations, mutexGroups, orGroups, xorGroups, rc)
	  val phi = None
	  val afm = new AttributedFeatureModel(afd, phi)
	  stopLast()
	  
	  stopLast() // End synthesis
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
	  // Separate features from attributes w.r.t. the knowledge
	  val (features, attributes) = knowledge.extractFeaturesAndAttributes(matrix, domains)
	  
	  // Remove dead features
	  val aliveFeatures = features.filter(feature => domains(feature.name).exists(knowledge.isTrue(feature, _)))
	  // FIXME : remove corresponding column in matrix
	  
	  (aliveFeatures, attributes)
	}

	/**
	 * Compute binary implications between the values of the matrix's columns
	 */
	def computeBinaryImplicationConstraints(matrix : ConfigurationMatrix, features : List[Feature], attributes : List[Attribute], columnDomains : Map[String, Set[String]], knowledge : Knowledge)
	: List[Constraint] = {

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
	      
	  val ioHandler = ProcessLogger(stdout => {}, stderr => {})
	  start("Sicstus")
	  val commandResult = reasonerCommand ! ioHandler
	  stopLast()
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
	    	
	    	val leftLabelIndex = matcher.group(1)
	    	val leftLabel = matrix.labels(leftLabelIndex.toInt - 1)
		    val leftValue = matcher.group(2)
		    
		    val rightLabelIndex = matcher.group(3)
		    val rightLabel = matrix.labels(rightLabelIndex.toInt - 1)
		    val includedValues = matcher.group(4).split(",").toList.filter(!_.isEmpty())
		    val excludedValues = matcher.group(5).split(",").toList.filter(!_.isEmpty())
		    
		    
		    val leftVariable = convertLabelToVariable(leftLabel, features, attributes)
		    val leftConstraint = convertLeftValueToConstraint(leftVariable, leftValue, invertedDictionaries, knowledge)
		    
		    val rightVariable = convertLabelToVariable(rightLabel, features, attributes) 
		    val rightConstraint = convertRightValuesToConstraint(rightVariable, includedValues, excludedValues, invertedDictionaries, knowledge)
		    
		    val constraint = Implies(leftConstraint, rightConstraint)

		    constraint match {
	    	  case Implies(_, True()) => None
	    	  case _ => Some(constraint)
	    	}
	    } else {
	    	println("Following line is not a correct output of Sicstus reasoner: " + line)
	    	None
	    }
	  }
	  
	  constraints.flatten.toList
	}
	
	/**
	 * Convert a cell to a feature or an attribute value depending on the nature of the column
	 */
	private def convertLabelToVariable(label : String, features : List[Feature], attributes : List[Attribute])
	: Variable = {
	  val feature = features.find(_.name == label)
	  if (feature.isDefined) {
    	feature.get
	  } else {
		val attribute = attributes.find(_.name == label)
		attribute.get
	  }
	}
	
	private def convertLeftValueToConstraint(variable : Variable, value : String, invertedDictionaries : collection.mutable.Map[String, Map[String,String]], knowledge : Knowledge)
	: Constraint = {
	  variable match {
	    case f : Feature => {
	      if (knowledge.isTrue(f, invertedDictionaries(f.name)(value))) {
		      f
		    } else {
		      Not(f)
		    }
	    }
	    case a : Attribute => Equal(a, invertedDictionaries(a.name)(value))
	  }
	}
	
	private def convertRightValuesToConstraint(variable : Variable, includedValues : List[String], excludedValues : List[String], invertedDictionaries : collection.mutable.Map[String, Map[String,String]], knowledge : Knowledge)
	: Constraint = {
	  variable match {
	    case f : Feature => {
	      if (includedValues.size == 2) {
	    	  True()
	      } else {
	    	  if (knowledge.isTrue(f, invertedDictionaries(f.name)(includedValues.head))) {
	    	    f
	    	  } else {
	    	    Not(f)
	    	  }
	      }
	    }
	    case a : Attribute => {
	      val originalIncludedValues = includedValues.map(invertedDictionaries(a.name)(_)).toSet
	      val originalExcludedValues = excludedValues.map(invertedDictionaries(a.name)(_)).toSet
	      And(IncludedIn(a, originalIncludedValues), Not(IncludedIn(a, originalExcludedValues)))
	    }
	  }
	}
	
	/**
	 * Compute binary implication graph and mutex graph
	 */
	def computeBinaryImplicationAndMutexGraph(features : List[Feature], constraints : List[Constraint])
	: (ImplicationGraph[Feature], ExclusionGraph[Feature]) = {
	  
	  val big = new ImplicationGraph[Feature]
	  features.foreach(big.addVertex(_))
	  
	  val mutexGraph = new ExclusionGraph[Feature]
	  features.foreach(mutexGraph.addVertex(_))
	  
	  for (constraint <- constraints) {
	    constraint match {
	      case Implies(f1 : Feature, f2 : Feature) => big.addEdge(f1, f2)
	      case Implies(f1 : Feature, Not(f2 : Feature)) => mutexGraph.addEdge(f1, f2)
	      case _ =>
	    }
	  }
	  
	  (big, mutexGraph)
	}
	
	/**
	 * Extract a particular hierarchy for the AFM
	 * @param : big : binary implication graph representing all legal hierarchies of the AFM
	 */
	def extractHierarchy(big : ImplicationGraph[Feature], knowledge : Knowledge): ImplicationGraph[Feature] = {
	  knowledge.selectHierarchy(big)
	}
	
	/**
	 * Place attributes in features
	 */
	def placeAttributes(features : List[Feature], attributes : List[Attribute], constraints : List[Constraint], knowledge : Knowledge) = {

	  // Compute legal positions for the attributes
	  val legalPositions = collection.mutable.Map.empty[Attribute, Set[Feature]]
	  for (attribute <- attributes) {
	    legalPositions(attribute) = features.toSet
	  }
	  
	  // If (not f => a = 0d), then the feature f is a legal position for the attribute a
	  // here we check the negation of this property to remove illegal positions
	  for (constraint <- constraints) {
	    constraint match {
	      case Implies(Not(f : Feature), And(IncludedIn(attribute, impliedValues), _)) =>
	        if (impliedValues.exists(_ != attribute.domain.nullValue)) {
	          legalPositions(attribute) = legalPositions(attribute) - f
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
	
	/**
	 * Compute mandatory relations in the hierarchy
	 */
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
	
	/**
	 * Compute all possible mutex groups
	 */
	def computeMutexGroups(mutexGraph : ExclusionGraph[Feature], hierarchy : ImplicationGraph[Feature], features : List[Feature])
	: List[MutexGroup] = {
	  val mutexGroups = ListBuffer.empty[MutexGroup]
	  val cliques = new ExclusionGraphUtil[Feature].cliques(mutexGraph)
	  for (parent <- features; clique <- cliques) {
	    val children = clique intersect hierarchy.children(parent)
	    if (children.size >= 2) {
	      mutexGroups += MutexGroup(parent, children.toSet)
	    }
	  }
	  
	  mutexGroups.toList
	}
	
	
	/**
	 * Compute all possible or groups
	 */
	def computeOrGroups(matrix : ConfigurationMatrix, hierarchy : ImplicationGraph[Feature], features : List[Feature], knowledge : Knowledge) 
	: List[OrGroup] = {
	  
	  // Convert matrix to DNF
	  val variables = features.map(feature => matrix.labels.indexOf(feature.name))  
	  
	  val clauses = for (configuration <- matrix.configurations) yield {
		  val clause = new Array[Int](variables.size)
		  for ((variable, index) <- variables.zipWithIndex) {
			  val valueIsTrue = knowledge.isTrue(features(index), configuration(variable))
			  
			  // a literal must not be equal to 0
			  val literal = if (valueIsTrue) {
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
	    	  orGroups += OrGroup(parent, children.toSet)
	      }
	      
	    }
	  }
 
	  orGroups.toList
	}
	
	/**
	 * Compute all possible Xor groups
	 */
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
	
	/**
	 * Compute cross tree binary implications between features
	 */
	def computeCrossTreeImplications(hierarchy: ImplicationGraph[Feature], big : ImplicationGraph[Feature], mandatoryRelations : List[Mandatory]) 
	: List[BinaryImplicationConstraint] = {
	  
	  // Compute represented implications by the hierarchy and the mandatory relations
	  val representedImplications = new ImplicationGraph[Feature]
	  hierarchy.vertices().foreach(representedImplications.addVertex(_))
	  for (edge <- hierarchy.edges()) {
		  val source = hierarchy.getEdgeSource(edge)
		  val target = hierarchy.getEdgeTarget(edge)
		  representedImplications.addEdge(source, target)
	  }
	  
	  for (relation <- mandatoryRelations) {
	    representedImplications.addEdge(relation.parent, relation.child)
	  }
	  
	  TransitiveClosure.INSTANCE.closeSimpleDirectedGraph(representedImplications)
	  
	  // Create cross tree implications 
	  val implies = ListBuffer.empty[BinaryImplicationConstraint]
	  for (edge <- big.edges()) {
		  val source = big.getEdgeSource(edge)
		  val target = big.getEdgeTarget(edge)
		  
		  if (!Option(representedImplications.findEdge(source, target)).isDefined) {
		    implies += BinaryImplicationConstraint(source, target)
		  }
	  }
	  
	  implies.toList
	}
	
	/**
	 * Compute cross tree binary exclusions between features
	 */
	def computeCrossTreeExcludes(mutexGraph : ExclusionGraph[Feature], mutexGroups : List[MutexGroup], xorGroups : List[XorGroup])
	: List[BinaryExclusionConstraint] = {
	  val excludes = ListBuffer.empty[BinaryExclusionConstraint]
	  
	  // Remove edges of mutex graph that are represented in mutex groups
	  val crossTreeMutex = mutexGraph.clone().asInstanceOf[ExclusionGraph[Feature]]
	  for (group <- (mutexGroups ::: xorGroups)) {
	    for (f1 <- group.children;
	    f2 <- group.children
	    if f1 != f2) {
	      crossTreeMutex.removeEdge(f1, f2)
	    }
	  }
	  
	  // Remaining edges are excludes cross tree constraints
	  for (edge <- crossTreeMutex.edgeSet()) {
	    val feature = crossTreeMutex.getEdgeSource(edge)
	    val excluded = crossTreeMutex.getEdgeTarget(edge)
	    excludes += new BinaryExclusionConstraint(feature, excluded)
	  }
	  
	  excludes.toList
	}
	
	/**
	 * Select non overlapping groups among mutex or and xor groups
	 * Mutex and Or groups than are also Xor groups are discarded to keep the maximality of the AFM
	 */
	def processOverlappingGroups(features : List[Feature], mutexGroups : List[MutexGroup], orGroups : List[OrGroup], xorGroups : List[XorGroup], knowledge : Knowledge)
	: (List[MutexGroup], List[OrGroup], List[XorGroup]) = {
	  val selectedMutex = collection.mutable.Set.empty[MutexGroup]
	  val selectedOr = collection.mutable.Set.empty[OrGroup]
	  val selectedXor = collection.mutable.Set.empty[XorGroup]
	
	  // Remove xor groups from mutex and or groups to keep the AFM maximal
	  def existInXorGroups(group : Relation) : Boolean = {
	    xorGroups.exists(xorGroup => 
	      (group.parent == xorGroup.parent)
	      && (group.children == xorGroup.children)
	    )
	  }
	  selectedMutex ++= mutexGroups.filterNot(existInXorGroups(_))
	  selectedOr ++= orGroups.filterNot(existInXorGroups(_))
	  selectedXor ++= xorGroups
	  
	  // Select overlapping groups according to the knowledge
	  val groups = collection.mutable.Set.empty[FeatureGroup]
	  groups ++= selectedMutex
	  groups ++= selectedOr
	  groups ++= selectedXor

	  // FIXME : not really a good algorithm for selecting overlapping groups
	  // because it depends on the order of the features
	  for (feature <- features) {
	    val groupsWithThisFeature = groups.filter(_.children.contains(feature))
	    
	    if (groupsWithThisFeature.size > 1) {
	      val selectedGroup = knowledge.selectOneGroup(groupsWithThisFeature.toSet)

	      // FIXME : we can try to keep the unselected groups by removing the feature	      
	      for (unselectedGroup <- groupsWithThisFeature if unselectedGroup != selectedGroup) {
	    	  groups -= unselectedGroup
	    	  unselectedGroup match {
		        case g : MutexGroup => selectedMutex -= g
		        case g : OrGroup => selectedOr -= g
		        case g : XorGroup => selectedXor -= g
		      }
	      }
	    }
	  }

	  (selectedMutex.toList, selectedOr.toList, selectedXor.toList)
	}
	
	def computeComplexCrossTreeConstraints(constraints : List[Constraint])
	: List[Constraint] = {
	  val crossTreeConstraints = ListBuffer.empty[Constraint]
	  crossTreeConstraints ++= constraints
	  crossTreeConstraints.toList
	}
}