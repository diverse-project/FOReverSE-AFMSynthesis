package foreverse.afmsynthesis.writer

import java.io.File
import java.util.HashSet
import scala.collection.JavaConversions.asScalaSet
import foreverse.afmsynthesis.afm._
import foreverse.afmsynthesis.afm.constraint._
import fr.familiar.attributedfm.domain.{StringDomain, SetIntegerDomain}
import fr.familiar.attributedfm.GenericAttribute
import fr.familiar.attributedfm.ExcludesDependency
import fr.familiar.attributedfm.ComplexConstraint
import fr.familiar.attributedfm.RequiresDependency

import collection.JavaConversions._

class ModelBasedFAMAWriter extends FAMAWriter {

  var afmToFAMA : Map[Feature, fr.familiar.attributedfm.Feature] = _
  var afd : AttributedFeatureDiagram = _
  
  override def write(afm : AttributedFeatureModel, file : File) {
	  val famaAFM = new fr.familiar.attributedfm.AttributedFeatureModel

	  // Create FAMA features
	  afd = afm.diagram
	  afmToFAMA = afd.features.map(f => (f -> new fr.familiar.attributedfm.Feature(filterVariableName(f.name)))).toMap
//	  for ((feature, famaFeature) <- afmToFAMA) {
//	    famaFeature.setName(feature.name)
//	  }
	  
	  writeHierarchy(famaAFM, afmToFAMA)
	  writeAttributes(famaAFM, afmToFAMA)
	  writeConstraints(famaAFM)
	  
	  val writer = new AttributedWriter
	  file.delete()
	  writer.writeFile(file.getAbsolutePath(), famaAFM)
  }

  private def filterVariableName(name : String): String = {
    val filteredName = name.replaceAll(" ", "_")
      .replaceAll("\"", "")
      .replaceAll("\\(", "")
      .replaceAll("\\)", "")
      .replaceAll("-", "")
      .replaceAll("\\.", "")
      .replaceAll("\\|", "")
      .replaceAll(",", "")
      .replaceAll("/", "")
      .replaceAll(";", "")
      .replaceAll("&", "")
      .replaceAll(":", "")
      .replaceAll("#", "")
      .replaceAll("$", "")
    if (filteredName.matches("\\d.*")) {
     "_" + filteredName
    } else {
      filteredName
    }
  }

  private def writeHierarchy(famaAFM : fr.familiar.attributedfm.AttributedFeatureModel, afmToFAMA : Map[Feature, fr.familiar.attributedfm.Feature]) {
    
    val hierarchy = afd.hierarchy
    val relations = afd.mandatoryRelations ::: 
				afd.mutexGroups ::: 
				afd.orGroups ::: 
				afd.xorGroups
    val roots = hierarchy.roots()
    assert(roots.size() == 1, "an AFM must have exactly one root")
    
    val root = roots.head
     
    // Set root
    val famaRoot = afmToFAMA(root)
    famaAFM.setRoot(famaRoot)
    
    // Set relations
    for (relation <- relations) {
      val famaRelation = new fr.familiar.attributedfm.Relation
      afmToFAMA(relation.parent).addRelation(famaRelation)
      relation.children.foreach(c => famaRelation.addDestination(afmToFAMA(c))) 
      
      val (cardInf, cardSup) = relation match {
        case Optional(_,_) => (0, 1)
        case Mandatory(_,_) => (1, 1)
        case MutexGroup(_,_) => (0, 1)
        case OrGroup(_,_) => (1, relation.children.size)
        case XorGroup(_,_) => (1, 1)
      }
//      println(relation.parent + " / " + relation.children + " : " + cardInf + ", " + cardSup)
      famaRelation.addCardinality(new fr.familiar.attributedfm.domain.Cardinality(cardInf, cardSup))
    }
    
    // Set optional relations if there were not already added
    for ((feature, famaFeature) <- afmToFAMA) {
      if ((feature != root) && (!relations.exists(_.children.contains(feature)))) {
        val parent = hierarchy.parents(feature).head
        val famaParent = afmToFAMA(parent)
        
        val famaRelation = new fr.familiar.attributedfm.Relation
        famaRelation.addDestination(famaFeature)
        famaParent.addRelation(famaRelation)
        famaRelation.addCardinality(new fr.familiar.attributedfm.domain.Cardinality(0, 1))
        
      }
    }
    

     
  }
  
  private def writeAttributes(famaAFM : fr.familiar.attributedfm.AttributedFeatureModel, afmToFAMA : Map[Feature, fr.familiar.attributedfm.Feature]) {
    for ((feature, famaFeature) <- afmToFAMA) {
    	for (attribute <- feature.attributes) {
    	  val name = filterVariableName(attribute.name)

        val integerDomain = attribute.hasIntegerDomain()

    	  val domain = if (integerDomain) {
          val intValues : java.util.Set[Integer] = new HashSet[Integer]
          attribute.domain.values.foreach(v => intValues.add(v.toInt))
          new SetIntegerDomain(intValues)
        } else {
          new StringDomain(attribute.domain.values.toList.map(filterVariableName(_)))
        }
    	  
    	  val nullValue = if (integerDomain) {
          attribute.domain.nullValue.toInt
        } else {
          filterVariableName(attribute.domain.nullValue)
        }

    	  val defaultValue = if (integerDomain) {
          attribute.domain.values.head.toInt
        } else {
          filterVariableName(attribute.domain.values.head)
        }
    	  
    	  val famaAttribute = new GenericAttribute(name, domain, nullValue, defaultValue)
    	  
    	  famaFeature.addAttribute(famaAttribute)
    	}
    }
  }
  
  private def writeConstraints(famaAFM : fr.familiar.attributedfm.AttributedFeatureModel) {

    for (constraint <- afd.constraints) {
      val famaConstraint = constraintToFAMA(constraint)
      famaAFM.addConstraint(famaConstraint)
    }
  }
  
  private def attributeToFama(attribute : Attribute) : String = {
      val feature = afd.features.find(_.attributes.contains(attribute))
      filterVariableName(feature.get.name) + "." + filterVariableName(attribute.name)
  }
  
  private def constraintToFAMA(constraint : Constraint) : fr.familiar.attributedfm.Constraint = {
    
    constraint match {
        case Implies(feature : Feature, implied : Feature) => 
          new RequiresDependency(afmToFAMA(feature), afmToFAMA(implied))
        case Excludes(feature : Feature, excluded : Feature) =>
          new ExcludesDependency(afmToFAMA(feature), afmToFAMA(excluded))
        case Implies(left, right) => 
          val constraintString = complexConstraintToFAMA(left) + " IMPLIES " + complexConstraintToFAMA(right)
          new ComplexConstraint(constraintString)
//        case _ => throw new UnsupportedOperationException
      }
    
  }
  
  private def complexConstraintToFAMA(constraint : Constraint) : String = {
    constraint match {
      case f : Feature => filterVariableName(f.name)
      case Not(f : Feature) => "NOT " + filterVariableName(f.name) + ""
      case attributeOperator : AttributeOperator => {
        val operator = attributeOperator match {
          case Equal(a, value) => " == "
          case Less(a, value) => " < "
          case LessOrEqual(a, value) => " <= "
          case Greater(a, value) => " > "
          case GreaterOrEqual(a, value) => " >= "
        }
        val value = attributeOperator.attribute.domain match {
          case d : IntegerDomain => attributeOperator.value
          case _ =>  '"' + filterVariableName(attributeOperator.value) + '"'
        }
        attributeToFama(attributeOperator.attribute) + operator + value
      }
      case _ => throw new UnsupportedOperationException
    }
  }
  
}