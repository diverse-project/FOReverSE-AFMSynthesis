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
import foreverse.afmsynthesis.afm.DomainKnowledge
import foreverse.afmsynthesis.afm.Mandatory
import foreverse.afmsynthesis.afm.MutexGroup
import foreverse.afmsynthesis.afm.OrGroup
import foreverse.afmsynthesis.afm.Relation
import foreverse.afmsynthesis.afm.XorGroup
import foreverse.afmsynthesis.afm.constraint.AttributeOperator
import foreverse.afmsynthesis.afm.constraint.AttributeValue
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
import foreverse.afmsynthesis.afm.constraint.And
import foreverse.afmsynthesis.afm.constraint.Implies
import foreverse.afmsynthesis.afm.constraint.Excludes
import foreverse.afmsynthesis.afm.Domain
import foreverse.afmsynthesis.afm.constraint.LessOrEqual
import scala.util.Random
import foreverse.afmsynthesis.afm.OrGroup
import java.util.HashSet
import foreverse.afmsynthesis.test.SynthesisMonitor
import foreverse.afmsynthesis.afm.constraint.Equal
import foreverse.afmsynthesis.afm.constraint.IncludedIn
import foreverse.afmsynthesis.afm.constraint.Less
import foreverse.afmsynthesis.afm.constraint.LessOrEqual
import foreverse.afmsynthesis.afm.constraint.GreaterOrEqual
import foreverse.afmsynthesis.afm.constraint.Greater
import scala.concurrent.future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.TimeoutException
import scala.concurrent.duration.DurationInt

class AFMSynthesizer extends PerformanceMonitor with SynthesisMonitor {
  
  
	def synthesize(matrix : ConfigurationMatrix, knowledge : DomainKnowledge, enableOrGroups : Boolean, orGroupTimeout : Option[Int], outputDirPath : String) : AttributedFeatureModel = {
	  resetTops() // Reset performance monitor
	  resetMetrics() // Reset synthesis monitor
	  
	  top("Synthesis")
	  
	
	  
	  // Extract the features, the attributes and their domains
	  top("Domain extraction")
	  val columnDomains = extractColumnDomains(matrix, knowledge)
	  top()
	  
	  top("Feature and attribute extraction")
	  val (features, attributes) = extractFeaturesAndAttributes(matrix, columnDomains, knowledge)
	  val domains = (for (attribute <- attributes) yield {
	    (attribute.name -> attribute.domain)
	  }).toMap
	  top()
	  setMetric("#features", features.size.toString)
	  setMetric("#attributes", attributes.size.toString)
	  
	  
	  
	  // Compute binary implications
	  top("Binary implications")
//	  val constraints = computeBinaryImplicationConstraints(matrix, features, attributes, columnDomains, knowledge, outputDirPath)
//	  
//	  top("alternative")
//	  val alternativeConstraints = alternativeComputeBinaryImplicationConstraints(matrix, features, attributes, columnDomains, knowledge, outputDirPath)
//	  top()
//	  assert(constraints.toSet == alternativeConstraints.toSet)

	  val constraints = alternativeComputeBinaryImplicationConstraints(matrix, features, attributes, columnDomains, knowledge, outputDirPath)
	  top()
	  
	  setMetric("#binary constraints", constraints.size.toString)
	  
	  
	  
	  // Define the hierarchy
	  top("Implication and Mutex graph")
	  val (big, mutexGraph) = computeBinaryImplicationAndMutexGraph(features, constraints)
	  top()
	  setMetric("#edges in BIG", big.edges().size.toString)
	  setMetric("#edges in Mutex graph", mutexGraph.edgeSet().size.toString)
	  
	  top("Hierarchy")
	  val hierarchy = extractHierarchy(big, knowledge)
	  top()
	  
	  top("Place attributes")
	  placeAttributes(features, attributes, constraints, knowledge)
	  top()
	  
	  // Compute variability information
	  top("Mandatory features")
	  val mandatoryRelations = computeMandatoryFeatures(big, hierarchy)
	  top()
	  setMetric("#mandatory relations", mandatoryRelations.size.toString)
	  
	  top("Feature groups")

	  top("Mutex")
	  var mutexGroups = computeMutexGroups(mutexGraph, hierarchy, features)
	  top()
	  
	  
	  top("Or")
	  var computedOrGroups = if (enableOrGroups && orGroupTimeout.isDefined) {
	    try {
			val orGroupComputation : Future[List[OrGroup]] = future {
			  computeOrGroups(matrix, hierarchy, features, knowledge)
		  	}
			
			Some(Await.result(orGroupComputation, orGroupTimeout.get.minutes))
		} catch {
		  case e : TimeoutException =>
		    synthesisLogger("timeout for Or groups")
		    None
		}
	  } else {
	    None
	  }
	  var orGroups = computedOrGroups.getOrElse(Nil)
	  top()
	  
	  top("Xor")
	  var xorGroups = if (computedOrGroups.isDefined) {
	    computeXOrGroups(mutexGroups, computedOrGroups.get)
	  } else {
	    computeXOrGroupsAlternative(mutexGroups, matrix, knowledge)
	  }
	  top()
	  
	  top("Group processing")
	  val selectedGroups = processOverlappingGroups(features, mutexGroups, orGroups, xorGroups, knowledge) 
	  mutexGroups = selectedGroups._1
	  orGroups = selectedGroups._2
	  xorGroups = selectedGroups._3
	  top()
	  
	  log("Feature groups")
	  setMetric("#mutex groups", mutexGroups.size.toString)
	  mutexGroups.foreach(log(_))
	  setMetric("#or groups", orGroups.size.toString)
	  orGroups.foreach(log(_))
	  setMetric("#xor groups", xorGroups.size.toString)
	  xorGroups.foreach(log(_))
	  
	  top() // End feature group computation
	  
	  // Compute constraints
	  top("Cross tree constraints")
	  top("Binary implies")
	  val implies = computeCrossTreeImplications(hierarchy, big, mandatoryRelations)
	  top()
	  setMetric("#implies", implies.size.toString)
	  
	  top("Binary excludes")
	  val excludes = computeCrossTreeExcludes(mutexGraph, mutexGroups, xorGroups)
	  top()
	  setMetric("#excludes", excludes.size.toString)
	  
	  top("Complex constraints")
	  val complexConstraints = computeComplexCrossTreeConstraints(features, attributes, constraints, knowledge)
	  top()
	  setMetric("#complex constraints", complexConstraints.size.toString)
	  
	  val rc = implies ::: excludes ::: complexConstraints
	  top()
	  setMetric("#cross tree constraints", rc.size.toString)
	  
	  // Create the attributed feature model
	  top("AFM construction")
	  val afd = new AttributedFeatureDiagram(features, hierarchy, mandatoryRelations, mutexGroups, orGroups, xorGroups, rc)
	  val phi = None
	  val afm = new AttributedFeatureModel(afd, phi)
	  top()
	  
	  top() // End synthesis
	  afm
	}
	
	
	/**
	 * Extract domains of columns from the matrix
	 */
	def extractColumnDomains(matrix : ConfigurationMatrix, knowledge : DomainKnowledge) : Map[String, Set[String]] = {
	  
	  val domains : Map[String, Set[String]] = (for ((label, index) <- matrix.labels.zipWithIndex) yield {
	    
	    val values : Set[String] = (for (configuration <- matrix.configurations) yield {
		  configuration(index)
		})(collection.breakOut)
		
		(label -> values)
	  })(collection.breakOut)
	  
	  domains
	}
	
	
	/**
	 * Extract features and attributes from the matrix
	 */
	def extractFeaturesAndAttributes(matrix : ConfigurationMatrix, domains : Map[String, Set[String]], knowledge : DomainKnowledge) 
	: (List[Feature], List[Attribute]) = {
	  // Separate features from attributes w.r.t. the knowledge
	  val (features, attributes) = knowledge.extractFeaturesAndAttributes(matrix, domains)
	  
	  // Remove dead features
	  val aliveFeatures = features.filter(feature => domains(feature.name).exists(knowledge.isTrue(feature, _)))
	  
	  if (aliveFeatures.size < features.size) {
	    val filteredLabels = matrix.labels.filter(label => aliveFeatures.exists(_.name == label))
	    val filteredConfigurations = for (configuration <- matrix.configurations) yield {
	      val filteredConfiguration = configuration.zipWithIndex.filter(
	          value => aliveFeatures.exists(_.name == matrix.labels(value._2)))
	      filteredConfiguration.map(_._1)
	      
	    }
	    matrix.labels = filteredLabels
	    matrix.configurations = filteredConfigurations
	  }
	  
	  (aliveFeatures, attributes)
	}

	/**
	 * Compute binary implications between the values of the matrix's columns
	 */
	def computeBinaryImplicationConstraints(matrix : ConfigurationMatrix, features : List[Feature], attributes : List[Attribute], columnDomains : Map[String, Set[String]], knowledge : DomainKnowledge, outputDirPath : String)
	: List[Constraint] = {

	  // Create dictionary of matrix values
	  val dictionaries : Map[String, Map[String, String]] =
		  (for ((label, labelIndex) <- matrix.labels.zipWithIndex) yield {
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
		    
		    (label -> dictionary.toMap) 		
		  })(collection.breakOut)
	  
	  // Convert matrix to reasoner format
	  val convertedConfigurations = matrix.configurations.map(configuration =>
	    for ((value, index) <- configuration.zipWithIndex) yield {
	      dictionaries(matrix.labels(index))(value)
	    }
	  )
	  
	  val convertedMatrix = new ConfigurationMatrix(matrix.labels, convertedConfigurations)
	  
	  // Write converted matrix to CSV
	  val convertedMatrixFile = new File(outputDirPath + "converted_matrix.csv")
	  //File.createTempFile("afmsynthesis_", ".csv")
	    //new File("output/convertedMatrix.csv")
	  val resultFile = new File(outputDirPath + "sicstus_output.txt") 
	    //File.createTempFile("afmsynthesis_", ".txt")
	  //new File("output/results.txt")
	  val writer = new CSVWriter(new FileWriter(convertedMatrixFile))
	  writer.writeRow(convertedMatrix.labels)
	  convertedMatrix.configurations.foreach(writer.writeRow(_))
	  writer.close()

	  // Compute binary implication constraints with a prolog reasoner
	  
	  // Run sicstus reasoner
	  // sicstus -f -l revfm.pl --goal main. -a configuration_matrix.csv results.txt
//	  val reasonerCommand = Seq("sicstus", 
//	      "-f", 
//	      "-l", 
//	      "sicstus_reasoner/revfm.pl", 
//	      "--goal", 
//	      "main.", 
//	      "-a", 
//	      convertedMatrixFile.getAbsolutePath(),
//	      resultFile.getAbsolutePath()
//	      )
	  
	  val reasonerCommand = Seq("sicstus", 
	      "-f", 
	      "-r", 
	      "sicstus_reasoner/revfm.sav", 
	      "--goal", 
	      "main.", 
	      "-a", 
	      convertedMatrixFile.getAbsolutePath(),
	      resultFile.getAbsolutePath()
	      )
	      
//	  val ioHandler = ProcessLogger(stdout => {println(stdout)}, stderr => {println(stderr)})
	  val ioHandler = ProcessLogger(stdout => {synthesisLogger(stdout)}, stderr => {synthesisLogger(stderr)})
	  top("Sicstus")
	  val commandResult = reasonerCommand ! ioHandler
	  top()
	  assert(commandResult == 0, {convertedMatrixFile.delete(); "Something went wrong with Sicstus program"})

	  // Parse the output of the reasoner  
	  // and convert it to a list of constraints over the features and attributes
	  val invertedDictionaries = dictionaries.map((kv) => (kv._1 -> kv._2.map(_.swap)))
	  
	  val pattern = Pattern.compile("Feat(\\d+)\\s=\\s(\\d+)\\s=>\\sFeat(\\d+)\\sin\\s\\[(.*)\\]\\sand\\snot\\sin\\s\\[(.*)\\]")
	  
	  val constraints = (for (line <- Source.fromFile(resultFile).getLines) yield {
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
	  })
	  
	  // Delete temporary files
//	  convertedMatrixFile.delete()
//	  resultFile.delete()
	  
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
	
	private def convertLeftValueToConstraint(variable : Variable, value : String, invertedDictionaries : Map[String, Map[String,String]], knowledge : DomainKnowledge)
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
	
	private def convertRightValuesToConstraint(variable : Variable, includedValues : List[String], excludedValues : List[String], invertedDictionaries : Map[String, Map[String,String]], knowledge : DomainKnowledge)
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
	
	def alternativeComputeBinaryImplicationConstraints(matrix : ConfigurationMatrix, features : List[Feature], attributes : List[Attribute], columnDomains : Map[String, Set[String]], knowledge : DomainKnowledge, outputDirPath : String)
	: List[Constraint] = {

	  var constraints = List.empty[Constraint]
	  
	  for (
	        (var1, index1) <- matrix.labels.zipWithIndex;
	    	(var2, index2) <- matrix.labels.zipWithIndex
	    	if var1 != var2
	  ) {
	    
	    val mapForConstraints = collection.mutable.Map.empty[(String, String, String), collection.mutable.Set[String]]
	    
	    // Compute constraints for these two variables
		for (configuration <- matrix.configurations) {
		    val value1 = configuration(index1)
		    val value2 = configuration(index2)
		    
		    val impliedValues = mapForConstraints.get((var1, value1, var2))
		    if (impliedValues.isDefined) {
		      impliedValues.get += value2
		    } else {
		      mapForConstraints += ((var1, value1, var2) -> collection.mutable.Set(value2))
		    }
		}
	    
		// Create final constraints
		for (((var1, value, var2), includedValues) <- mapForConstraints) {
		  // Convert left part to constraint
		  val constraintVar1 = convertLabelToVariable(var1, features, attributes)
		    
		  val leftConstraint = constraintVar1 match {
		   	case f : Feature => {
			    if (knowledge.isTrue(f, value)) {
			      f
			    } else {
				     Not(f)
				}
			  }
			case a : Attribute => Equal(a, value)
		  }
		    
		  // Convert right part to constraint
		  val constraintVar2 = convertLabelToVariable(var2, features, attributes)
		    
		  val rightConstraint = constraintVar2 match {
			  case f : Feature => {
			    if (includedValues.size == 2) {
			   	  True()
			    } else {
			   	  if (knowledge.isTrue(f, includedValues.head)) {
			   	    f
			  	  } else {
			   	    Not(f)
			   	  }
			    }
			  }
			  case a : Attribute => {
			    val excludedValues = columnDomains(var2).diff(includedValues)
			    And(IncludedIn(a, includedValues.toSet), Not(IncludedIn(a, excludedValues)))
			  }
		  }
		    
		  // Filter trivial constraints
		    
		  val constraint = Implies(leftConstraint, rightConstraint)
		    
		  constraint match {
		  	  case Implies(_, True()) => 
		   	  case _ => constraints = constraint :: constraints
		  }
		}
	  }
	    
	  constraints
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
	def extractHierarchy(big : ImplicationGraph[Feature], knowledge : DomainKnowledge): ImplicationGraph[Feature] = {
	  knowledge.selectHierarchy(big)
	}
	
	/**
	 * Place attributes in features
	 */
	def placeAttributes(features : List[Feature], attributes : List[Attribute], constraints : List[Constraint], knowledge : DomainKnowledge) = {

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
	 * Compute all possible or groups with prime implicates (She et al., 2014)
	 */
	def computeOrGroups(matrix : ConfigurationMatrix, hierarchy : ImplicationGraph[Feature], features : List[Feature], knowledge : DomainKnowledge) 
	: List[OrGroup] = {
	  
	  // Convert matrix to DNF
//	  top("Conversion to DNF")
	  
	  val variables = features.map(feature => {
	    val index = matrix.labels.indexOf(feature.name)
	    val variable = index + 1  // a variable must not be equal to 0
	    (feature, index, variable)  
	  })
	  
	  val clauses = for (configuration <- matrix.configurations) yield {
		  val clause = new Array[Int](variables.size)
		  
		  for (((feature, index, variable), clauseIndex) <- variables.zipWithIndex) {
			  val valueIsTrue = knowledge.isTrue(feature, configuration(index))
			  
			  val literal = if (valueIsTrue) {
			    variable
			  }  else {
			    - variable
			  }
			  clause(clauseIndex) = literal
		  }

		  new DNFClause(clause)
	  }
	  
	  val dnf = new DNF(clauses)
	  
//	  top()
	  
	  // Compute Or groups
//	  top("Compute")
	  val orGroups = ListBuffer.empty[OrGroup]
	  for ((parent, index, variable) <- variables) {
	    val children = hierarchy.children(parent)

	    val reducedDNF = reduceDNF(dnf, parent, children.toSet, variables)
	    val computedOrGroups = reducedDNF.getOrGroups(variable)
	    
	    for (orGroup <- computedOrGroups) {
	      val literals = orGroup.getLiterals()
	      val groupChildren = for (literal <- literals) yield {
	        val featureIndex = literal.toInt.abs - 1
	        val label = matrix.labels(featureIndex)
	        val feature = features.find(_.name == label).get
	        feature
	      }
	      
	      // Filter groups that are not possible with this hierarchy
	      if (children.containsAll(groupChildren)) {
	    	  orGroups += OrGroup(parent, groupChildren.toSet)
	      }
	      
	    }
	  }
//	  top()
	  
	  orGroups.toList
	}
	
	/**
	 * Reduce DNF formula by removing all variables that are not the parent or its children
	 */
	def reduceDNF(dnf : DNF, parent : Feature, children : Set[Feature], variables : List[(Feature, Int, Int)]) : DNF = {
	  val unrelatedVariables = new HashSet[Integer] 
	  
	  for ((feature, index, variable) <- variables) yield {
	    if (feature != parent && !children.contains(feature)) {
	      unrelatedVariables.add(variable)
	    }
	  }
	  dnf.eliminateVariables(unrelatedVariables)
	}

	
	/**
	 * Compute all possible Xor groups based on the computation of mutex and or groups
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
	 * Compute all possible Xor groups based on the computation of mutex groups only
	 */
	def computeXOrGroupsAlternative(mutexGroups : List[MutexGroup], matrix : ConfigurationMatrix, knowledge : DomainKnowledge) = {
	  val xorGroups = ListBuffer.empty[XorGroup]
	  
	  for (mtxG <- mutexGroups) {
	      val parent = mtxG.parent
	      val children = mtxG.children
	      val labels = matrix.labels
	      val parentIndex = labels.indexOf(parent.name)
	      val childrenIndexes = children.map(c => (c -> labels.indexOf(c.name))).toMap
	      
	      // Check that there is no configuration with the parent but no children
	      // In that case, the mutex group is a Xor groups
	      val isXor = !matrix.configurations.exists(configuration =>
	      	knowledge.isTrue(parent, configuration(parentIndex)) 
	      	&& children.forall(child => !knowledge.isTrue(child, configuration(childrenIndexes(child))))
	      )
	      
	      if (isXor) {
	        xorGroups += XorGroup(parent, children)
	      }
	    }
	  
	  xorGroups.toList
	}
	
	/**
	 * Compute cross tree binary implications between features
	 */
	def computeCrossTreeImplications(hierarchy: ImplicationGraph[Feature], big : ImplicationGraph[Feature], mandatoryRelations : List[Mandatory]) 
	: List[Implies] = {
	  
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
	  val implies = ListBuffer.empty[Implies]
	  for (edge <- big.edges()) {
		  val source = big.getEdgeSource(edge)
		  val target = big.getEdgeTarget(edge)
		  
		  if (!Option(representedImplications.findEdge(source, target)).isDefined) {
		    implies += Implies(source, target)
		  }
	  }
	  
	  implies.toList
	}
	
	/**
	 * Compute cross tree binary exclusions between features
	 */
	def computeCrossTreeExcludes(mutexGraph : ExclusionGraph[Feature], mutexGroups : List[MutexGroup], xorGroups : List[XorGroup])
	: List[Excludes] = {
	  val excludes = ListBuffer.empty[Excludes]
	  
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
	    excludes += new Excludes(feature, excluded)
	  }
	  
	  excludes.toList
	}
	
	/**
	 * Select non overlapping groups among mutex or and xor groups
	 * Mutex and Or groups than are also Xor groups are discarded to keep the maximality of the AFM
	 */
	def processOverlappingGroups(features : List[Feature], mutexGroups : List[MutexGroup], orGroups : List[OrGroup], xorGroups : List[XorGroup], knowledge : DomainKnowledge)
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
	
	/**
	 * Compute complex constraints that can be integrated in RC
	 */
	def computeComplexCrossTreeConstraints(feature : List[Feature], attributes : List[Attribute], constraints : List[Constraint], knowledge : DomainKnowledge)
	: List[Constraint] = {
		val crossTreeConstraints = ListBuffer.empty[Constraint]
		crossTreeConstraints ++= computeCTCFromConstraints(constraints)
		crossTreeConstraints ++= computeCTCByAggregation(attributes, constraints, knowledge)
		crossTreeConstraints.toList
	}
	
	/**
	 * Compute complex CTC directly from simple constraints
	 * If the right part of a generated constraint can be represented with LessOrEqual, its equivalent is created
	 */
	private def computeCTCFromConstraints(constraints : List[Constraint]) : List[Constraint] = {
	  val crossTreeConstraints = ListBuffer.empty[Constraint]
	  
	  for (constraint <- constraints) {
	    val ctc = constraint match {
	      case Implies(left, And(IncludedIn(attribute, values), _)) => 
	        val max = findUpperBound(attribute.domain, values)
	        if (max.isDefined) {
	          Some(Implies(left, LessOrEqual(attribute, max.get)))
	        } else {
	          None
	        }
	        
	      case _ => None
	    }
	    
	    if(ctc.isDefined) {
	    	crossTreeConstraints += ctc.get 
	    }
	    
	  }
	  crossTreeConstraints.toList
	}
	
	/**
	 * Check that the set of values is the lower part of the domain
	 * If it is true, it returns its upper bound
	 */
	def findUpperBound(domain : Domain, values : Set[String]) : Option[String] =  {
		if (!values.isEmpty) {
			val sortedDomainValues = domain.values.toList.sortWith(domain.lessThan)
			val sortedValues = values.toList.sortWith(domain.lessThan)
			val max = sortedValues.last
			if (sortedDomainValues.startsWith(sortedValues) && max != sortedDomainValues.last) {
				Some(max)
			} else {
				None
			}
		} else {
		  None
		}
		
	}
	
	/**
	 * Check that the set of values is the upper part of the domain
	 * If it is true, it returns its lower bound
	 */
	def findLowerBound(domain : Domain, values : Set[String]) : Option[String] =  {
	  if (!values.isEmpty) {
			val sortedDomainValues = domain.values.toList.sortWith(domain.lessThan)
			val sortedValues = values.toList.sortWith(domain.lessThan)
			val min = sortedValues.head
			if (sortedDomainValues.endsWith(sortedValues) && min != sortedDomainValues.head) {
				Some(min)
			} else {
				None
			}
		} else {
		  None
		}
	}
	
	/**
	 * Aggregate constraints according to a bound defined by the domain knowledge
	 */
	private def computeCTCByAggregation(
	    attributes : List[Attribute], 
	    constraints : List[Constraint], 
	    knowledge : DomainKnowledge) 
	: List[Constraint] = {
	  val crossTreeConstraints = ListBuffer.empty[Constraint]
	  
	  // Aggregate generated constraints
	  val constraintBounds = attributes.map(a => a -> knowledge.getConstraintBound(a)).toMap
	  
	  val attributePairs = collection.mutable.Set.empty[(Attribute, Attribute)]
	  val lessMap = collection.mutable.Map.empty[(Attribute, Attribute), (Set[String], Set[String])]
	  val greaterMap = collection.mutable.Map.empty[(Attribute, Attribute), (Set[String], Set[String])]
	  val equalMap = collection.mutable.Map.empty[(Attribute, Attribute), (Set[String], Set[String])]
	  
	  for (constraint <- constraints) {
	    constraint match {
	      case Implies(Equal(a1, v1), And(IncludedIn(a2, included), Not(IncludedIn(_, excluded)))) => {
	        val attributePair = (a1, a2) 
	        attributePairs += attributePair
	        
	        val bound = constraintBounds(a1)
	        
	        val relatedMap = if (v1.toInt < bound.toInt) {
		      lessMap
	        } else if (v1.toInt > bound.toInt) {
	          greaterMap
	        } else {
	          equalMap
	        }
	        
	        val (prevIncluded, prevExcluded) = relatedMap.getOrElse(attributePair, (Set.empty[String], a2.domain.values))
		    relatedMap += (attributePair -> (prevIncluded union included, prevExcluded intersect excluded))  
		    
	      }
	      
	      case _ => 
	    }
	  }
	  
	  // Create complex constraints if possible
	  for ((a1, a2) <- attributePairs) {
	    val bound = constraintBounds(a1)
	    val (lessIncluded, lessExcluded) = lessMap.getOrElse((a1, a2), (Set.empty[String], a2.domain.values))
	    val (greaterIncluded, greaterExcluded) = greaterMap.getOrElse((a1, a2), (Set.empty[String], a2.domain.values))
	    val (equalIncluded, equalExcluded) = equalMap.getOrElse((a1, a2), (Set.empty[String], a2.domain.values))
	    
	    val a1Values = a1.domain.values.toList
	    val sortedA1Values = a1Values.sortWith(a1.domain.lessThan)
	    
	    addComplexCrossTreeConstraint(crossTreeConstraints, a1, a2, lessIncluded, lessExcluded, Less(a1, bound))
	    addComplexCrossTreeConstraint(crossTreeConstraints, a1, a2, greaterIncluded, greaterExcluded, Greater(a1, bound))
	    if (sortedA1Values.head != bound) {
	    	addComplexCrossTreeConstraint(crossTreeConstraints, a1, a2, lessIncluded union equalIncluded, lessExcluded intersect equalExcluded, LessOrEqual(a1, bound))
	    }
	    if (sortedA1Values.last != bound) {
	    	addComplexCrossTreeConstraint(crossTreeConstraints, a1, a2, greaterIncluded union equalIncluded, greaterExcluded intersect equalExcluded, GreaterOrEqual(a1, bound))  
	    }
	    
	    
	  }
	  
	  crossTreeConstraints.toList
	}
	
	/**
	 * Add a complex CTC if "included" can be represented as a LessOrEqual or GreaterOrEqual relation
	 */
	def addComplexCrossTreeConstraint(
	    crossTreeConstraints : ListBuffer[Constraint], 
	    a1 : Attribute, 
	    a2 : Attribute, 
	    included : Set[String], 
	    excluded : Set[String], 
	    leftConstraint : Constraint) {
	  
	    val upperBound = findUpperBound(a2.domain, included)
	    val lowerBound = findLowerBound(a2.domain, included)
	    
	    val constraint = if (upperBound.isDefined) {
	      if (included.size == 1) {
	        Some(Implies(leftConstraint, Equal(a2, upperBound.get)))
	      } else {
	        Some(Implies(leftConstraint, LessOrEqual(a2, upperBound.get)))
	      }
	    } else if (lowerBound.isDefined) {
	      if (included.size == 1) {
	        Some(Implies(leftConstraint, Equal(a2, lowerBound.get)))
	      } else {
	        Some(Implies(leftConstraint, GreaterOrEqual(a2, lowerBound.get)))
	      }
	    } else {
	    	None
	    }
	    
	    if (constraint.isDefined) {
	      crossTreeConstraints += constraint.get
	    }
		
	}
	
}